/* C++ implementation of a pure C API for emitting diagnostics.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#define INCLUDE_MAP
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "intl.h"
#include "diagnostic.h"
#include "diagnostics/color.h"
#include "diagnostics/file-cache.h"
#include "diagnostics/url.h"
#include "diagnostics/metadata.h"
#include "diagnostics/paths.h"
#include "diagnostics/client-data-hooks.h"
#include "diagnostics/sarif-sink.h"
#include "diagnostics/text-sink.h"
#include "diagnostics/output-spec.h"
#include "diagnostics/digraphs.h"
#include "diagnostics/state-graphs.h"
#include "diagnostics/logical-locations.h"
#include "diagnostics/dumping.h"
#include "diagnostics/changes.h"
#include "libgdiagnostics.h"
#include "libgdiagnostics-private.h"
#include "pretty-print-format-impl.h"
#include "pretty-print-markup.h"
#include "auto-obstack.h"

class owned_nullable_string
{
public:
  owned_nullable_string () : m_str (nullptr) {}
  owned_nullable_string (const char *str)
  : m_str (str ? ::xstrdup (str) : nullptr)
  {
  }
  owned_nullable_string (const owned_nullable_string &other)
  : m_str (other.xstrdup ())
  {
  }
  owned_nullable_string (owned_nullable_string &&other)
  {
    m_str = other.m_str;
    other.m_str = nullptr;
  }

  ~owned_nullable_string ()
  {
    free (m_str);
  }

  void set (const char *str)
  {
    free (m_str);
    m_str = str ? ::xstrdup (str) : nullptr;
  }

  const char *get_str () const { return m_str; }

  char *xstrdup () const
  {
    return m_str ? ::xstrdup (m_str) : nullptr;
  }

  bool
  operator< (const owned_nullable_string &other) const
  {
    if (m_str && other.m_str)
      return strcmp (m_str, other.m_str) < 0;
    if (m_str == nullptr && other.m_str != nullptr)
      return true;
    return false;
  }

private:
  char *m_str;
};

class content_buffer
{
public:
  content_buffer (const char *data, size_t sz)
  : m_data (xmalloc (sz)),
    m_sz (sz)
  {
    memcpy (m_data, data, sz);
  }
  ~content_buffer ()
  {
    free (m_data);
  }

  void *m_data;
  size_t m_sz;
};

/* This has to be a "struct" as it is exposed in the C API.  */

struct diagnostic_file
{
  diagnostic_file (diagnostic_manager &mgr,
		   const char *name,
		   const char *sarif_source_language)
  : m_mgr (mgr),
    m_name (name),
    m_sarif_source_language (sarif_source_language)
  {
  }

  const char *get_name () const { return m_name.get_str (); }
  const char *get_sarif_source_language () const
  {
    return m_sarif_source_language.get_str ();
  }

  const content_buffer *
  get_content () const
  {
    return m_content.get ();
  }
  void set_buffered_content (const char *buf, size_t sz);

private:
  diagnostic_manager &m_mgr;
  owned_nullable_string m_name;
  owned_nullable_string m_sarif_source_language;
  std::unique_ptr<content_buffer> m_content;
};

/* This has to be a "struct" as it is exposed in the C API.  */

struct diagnostic_physical_location
{
  diagnostic_physical_location (diagnostic_manager *mgr,
				location_t inner)
  : m_mgr (mgr),
    m_inner (inner)
  {}

  diagnostic_file *get_file () const;

  diagnostic_manager *m_mgr;
  location_t m_inner;
};

static location_t
as_location_t (const diagnostic_physical_location *loc)
{
  if (!loc)
    return UNKNOWN_LOCATION;
  return loc->m_inner;
}

/* This has to be a "struct" as it is exposed in the C API.  */

struct diagnostic_logical_location
{
  diagnostic_logical_location (enum diagnostic_logical_location_kind_t kind,
			       const diagnostic_logical_location *parent,
			       const char *short_name,
			       const char *fully_qualified_name,
			       const char *decorated_name)
  : m_kind (kind),
    m_parent (parent),
    m_short_name (short_name),
    m_fully_qualified_name (fully_qualified_name),
    m_decorated_name (decorated_name)
  {
  }

  bool
  operator< (const diagnostic_logical_location &other) const
  {
    if (m_kind < other.m_kind)
      return true;
    if (m_parent < other.m_parent)
      return true;
    if (m_short_name < other.m_short_name)
      return true;
    if (m_fully_qualified_name < other.m_fully_qualified_name)
      return true;
    if (m_decorated_name < other.m_decorated_name)
      return true;

    return false;
  }

  enum diagnostic_logical_location_kind_t m_kind;
  const diagnostic_logical_location *m_parent;
  owned_nullable_string m_short_name;
  owned_nullable_string m_fully_qualified_name;
  owned_nullable_string m_decorated_name;
};

static diagnostic_event_id
as_diagnostic_event_id (diagnostics::paths::event_id_t id)
{
  return id.zero_based ();
}

class sink
{
protected:
  sink (diagnostic_manager &mgr) : m_mgr (mgr) {}

  diagnostic_manager &m_mgr;
};

/* This has to be a "struct" as it is exposed in the C API.  */

struct diagnostic_text_sink : public sink
{
public:
  diagnostic_text_sink (diagnostic_manager &mgr,
			FILE *dst_stream,
			enum diagnostic_colorize colorize);

  diagnostics::source_printing_options &get_source_printing_options ()
  {
    return m_source_printing;
  }

  void
  set_colorize (enum diagnostic_colorize colorize);

  static void
  text_starter (diagnostics::text_sink &text_output,
		const diagnostics::diagnostic_info *diagnostic);

private:
  diagnostics::text_sink *m_inner_sink; // borrowed from dc
  diagnostics::source_printing_options m_source_printing;
};

/* A token_printer that makes a deep copy of the pp_token_list
   into another obstack.  */

class copying_token_printer : public token_printer
{
public:
  copying_token_printer (obstack &dst_obstack,
			 pp_token_list &dst_token_list)
  : m_dst_obstack (dst_obstack),
    m_dst_token_list (dst_token_list)
  {
  }

  void
  print_tokens (pretty_printer *,
		const pp_token_list &tokens) final override
  {
    for (auto iter = tokens.m_first; iter; iter = iter->m_next)
      switch (iter->m_kind)
	{
	default:
	  gcc_unreachable ();

	case pp_token::kind::text:
	  {
	    const pp_token_text *sub = as_a <const pp_token_text *> (iter);
	    /* Copy the text, with null terminator.  */
	    obstack_grow (&m_dst_obstack, sub->m_value.get (),
			  strlen (sub->m_value.get ()) + 1);
	    m_dst_token_list.push_back_text
	      (label_text::borrow (XOBFINISH (&m_dst_obstack,
					      const char *)));
	  }
	  break;

	case pp_token::kind::begin_color:
	  {
	    pp_token_begin_color *sub = as_a <pp_token_begin_color *> (iter);
	    /* Copy the color, with null terminator.  */
	    obstack_grow (&m_dst_obstack, sub->m_value.get (),
			  strlen (sub->m_value.get ()) + 1);
	    m_dst_token_list.push_back<pp_token_begin_color>
	      (label_text::borrow (XOBFINISH (&m_dst_obstack,
					      const char *)));
	  }
	  break;
	case pp_token::kind::end_color:
	  m_dst_token_list.push_back<pp_token_end_color> ();
	  break;

	case pp_token::kind::begin_quote:
	  m_dst_token_list.push_back<pp_token_begin_quote> ();
	  break;
	case pp_token::kind::end_quote:
	  m_dst_token_list.push_back<pp_token_end_quote> ();
	  break;

	case pp_token::kind::begin_url:
	  {
	    pp_token_begin_url *sub = as_a <pp_token_begin_url *> (iter);
	    /* Copy the URL, with null terminator.  */
	    obstack_grow (&m_dst_obstack, sub->m_value.get (),
			  strlen (sub->m_value.get ()) + 1);
	    m_dst_token_list.push_back<pp_token_begin_url>
	      (label_text::borrow (XOBFINISH (&m_dst_obstack,
					      const char *)));
	  }
	  break;
	case pp_token::kind::end_url:
	  m_dst_token_list.push_back<pp_token_end_url> ();
	  break;

	case pp_token::kind::event_id:
	  {
	    pp_token_event_id *sub = as_a <pp_token_event_id *> (iter);
	    m_dst_token_list.push_back<pp_token_event_id> (sub->m_event_id);
	  }
	  break;

	case pp_token::kind::custom_data:
	  /* These should have been eliminated by replace_custom_tokens.  */
	  gcc_unreachable ();
	  break;
	}
  }

private:
  obstack &m_dst_obstack;
  pp_token_list &m_dst_token_list;
};

class sarif_sink : public sink
{
public:
  sarif_sink (diagnostic_manager &mgr,
	      FILE *dst_stream,
	      const diagnostic_file *main_input_file,
	      const diagnostics::sarif_generation_options &sarif_gen_opts);
};

struct diagnostic_message_buffer
{
  diagnostic_message_buffer ()
  : m_tokens (m_obstack)
  {
  }

  diagnostic_message_buffer (const char *gmsgid,
			     va_list *args)
  : m_tokens (m_obstack)
  {
    text_info text (gmsgid, args, errno);
    pretty_printer pp;
    pp.set_output_stream (nullptr);
    copying_token_printer tok_printer (m_obstack, m_tokens);
    pp.set_token_printer (&tok_printer);
    pp_format (&pp, &text);
    pp_output_formatted_text (&pp, nullptr);
  }


  std::string to_string () const;

  auto_obstack m_obstack;
  pp_token_list m_tokens;
};

/* A pp_element subclass that replays the saved tokens in a
   diagnostic_message_buffer.  */

class pp_element_message_buffer : public pp_element
{
public:
  pp_element_message_buffer (diagnostic_message_buffer &msg_buf)
    : m_msg_buf (msg_buf)
  {
  }

  void add_to_phase_2 (pp_markup::context &ctxt) final override
  {
    /* Convert to text, possibly with colorization, URLs, etc.  */
    for (auto iter = m_msg_buf.m_tokens.m_first; iter; iter = iter->m_next)
      switch (iter->m_kind)
	{
	default:
	  gcc_unreachable ();

	case pp_token::kind::text:
	  {
	    pp_token_text *sub = as_a <pp_token_text *> (iter);
	    pp_string (&ctxt.m_pp, sub->m_value.get ());
	    ctxt.push_back_any_text ();
	  }
	  break;

	case pp_token::kind::begin_color:
	  {
	    pp_token_begin_color *sub = as_a <pp_token_begin_color *> (iter);
	    ctxt.begin_highlight_color (sub->m_value.get ());
	  }
	  break;
	case pp_token::kind::end_color:
	  ctxt.end_highlight_color ();
	  break;

	case pp_token::kind::begin_quote:
	  ctxt.begin_quote ();
	  break;
	case pp_token::kind::end_quote:
	  ctxt.end_quote ();
	  break;

	case pp_token::kind::begin_url:
	  {
	    pp_token_begin_url *sub = as_a <pp_token_begin_url *> (iter);
	    ctxt.begin_url (sub->m_value.get ());
	  }
	  break;
	case pp_token::kind::end_url:
	  ctxt.end_url ();
	  break;

	case pp_token::kind::event_id:
	  {
	    pp_token_event_id *sub = as_a <pp_token_event_id *> (iter);
	    gcc_assert (sub->m_event_id.known_p ());
	    ctxt.add_event_id (sub->m_event_id);
	  }
	  break;

	case pp_token::kind::custom_data:
	  /* We don't have a way of handling custom_data tokens here.  */
	  gcc_unreachable ();
	  break;
	}
  }

private:
  diagnostic_message_buffer &m_msg_buf;
};

/* Helper for the linemap code.  */

static size_t
round_alloc_size (size_t s)
{
  return s;
}

class impl_logical_location_manager
  : public diagnostics::logical_locations::manager
{
public:
  using key = diagnostics::logical_locations::key;
  using kind = diagnostics::logical_locations::kind;

  static const diagnostic_logical_location *
  ptr_from_key (key k)
  {
    return k.cast_to<const diagnostic_logical_location *> ();
  }

  static key
  key_from_ptr (const diagnostic_logical_location *ptr)
  {
    return key::from_ptr (ptr);
  }

  void dump (FILE *outfile, int indent) const final override
  {
    diagnostics::dumping::emit_heading
      (outfile, indent, "impl_logical_location_manager");
  }

  const char *get_short_name (key k) const final override
  {
    if (auto loc = ptr_from_key (k))
      return loc->m_short_name.get_str ();
    else
      return nullptr;
  }

  const char *get_name_with_scope (key k) const final override
  {
    if (auto loc = ptr_from_key (k))
      return loc->m_fully_qualified_name.get_str ();
    else
      return nullptr;
  }

  const char *get_internal_name (key k) const final override
  {
    if (auto loc = ptr_from_key (k))
      return loc->m_decorated_name.get_str ();
    else
      return nullptr;
  }

  kind get_kind (key k) const final override
  {
    auto loc = ptr_from_key (k);
    gcc_assert (loc);
    switch (loc->m_kind)
      {
      default:
	gcc_unreachable ();

      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION:
	return kind::function;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_MEMBER:
	return kind::member;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_MODULE:
	return kind::module_;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_NAMESPACE:
	return kind::namespace_;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_TYPE:
	return kind::type;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_RETURN_TYPE:
	return kind::return_type;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_PARAMETER:
	return kind::parameter;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_VARIABLE:
	return kind::variable;

      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_ELEMENT:
	return kind::element;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_ATTRIBUTE:
	return kind::attribute;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_TEXT:
	return kind::text;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_COMMENT:
	return kind::comment;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_PROCESSING_INSTRUCTION:
	return kind::processing_instruction;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_DTD:
	return kind::dtd;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_DECLARATION:
	return kind::declaration;

      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_OBJECT:
	  return kind::object;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_ARRAY:
	return kind::array;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_PROPERTY:
	return kind::property;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_VALUE:
	return kind::value;
      }
  }

  label_text get_name_for_path_output (key k) const final override
  {
    auto loc = ptr_from_key (k);
    gcc_assert (loc);
    return label_text::borrow (loc->m_short_name.get_str ());
  }

  key get_parent (key k) const final override
  {
    auto loc = ptr_from_key (k);
    gcc_assert (loc);
    return key_from_ptr (loc->m_parent);
  }
};

class impl_diagnostic_client_data_hooks : public diagnostics::client_data_hooks
{
public:
  impl_diagnostic_client_data_hooks (diagnostic_manager &mgr)
  : m_mgr (mgr)
  {}

  const diagnostics::client_version_info *
  get_any_version_info () const final override;

  const diagnostics::logical_locations::manager *
  get_logical_location_manager () const final override
  {
    return &m_logical_location_manager;
  }
  diagnostics::logical_locations::key
  get_current_logical_location () const final override;

  const char * maybe_get_sarif_source_language (const char *filename)
    const final override;
  void
  add_sarif_invocation_properties (diagnostics::sarif_object &invocation_obj)
    const final override;

private:
  diagnostic_manager &m_mgr;
  impl_logical_location_manager m_logical_location_manager;
};

class impl_client_version_info : public diagnostics::client_version_info
{
public:
  const char *get_tool_name () const final override
  {
    return m_name.get_str ();
  }

  char *maybe_make_full_name () const final override
  {
    return m_full_name.xstrdup ();
  }

  const char *get_version_string () const final override
  {
    return m_version.get_str ();
  }

  char *maybe_make_version_url () const final override
  {
    return m_version_url.xstrdup ();
  }

  void for_each_plugin (plugin_visitor &) const final override
  {
    // No-op.
  }

  owned_nullable_string m_name;
  owned_nullable_string m_full_name;
  owned_nullable_string m_version;
  owned_nullable_string m_version_url;
};

/* This has to be a "struct" as it is exposed in the C API.  */

struct diagnostic_manager
{
public:
  diagnostic_manager ()
  : m_current_diag (nullptr),
    m_prev_diag_logical_loc (nullptr),
    m_debug_physical_locations (false)
  {
    linemap_init (&m_line_table, BUILTINS_LOCATION);
    m_line_table.m_reallocator = xrealloc;
    m_line_table.m_round_alloc_size = round_alloc_size;
    m_line_table.default_range_bits = line_map_suggested_range_bits;

    diagnostic_initialize (&m_dc, 0);
    m_dc.remove_all_output_sinks ();

    /* Get defaults from environemt.  These might be
       overridden by individual sinks.  */
    diagnostic_color_init (&m_dc, DIAGNOSTICS_COLOR_AUTO);
    diagnostic_urls_init (&m_dc);

    m_dc.set_show_cwe (true);
    m_dc.set_show_rules (true);
    m_dc.m_show_column = true;
    auto &source_printing_opts = m_dc.get_source_printing_options ();
    source_printing_opts.enabled = true;
    source_printing_opts.colorize_source_p = true;

    /* We don't currently expose a way for clients to manipulate the
       following.  */
    source_printing_opts.show_labels_p = true;
    source_printing_opts.show_line_numbers_p = true;
    source_printing_opts.min_margin_width = 6;
    m_dc.set_path_format (DPF_INLINE_EVENTS);

    m_dc.m_client_aux_data = this;
    m_dc.set_client_data_hooks
      (std::make_unique<impl_diagnostic_client_data_hooks> (*this));

    diagnostics::text_starter (&m_dc) = diagnostic_text_sink::text_starter;

    m_change_set
      = std::make_unique <diagnostics::changes::change_set>
	  (m_dc.get_file_cache ());
  }

  ~diagnostic_manager ()
  {
    diagnostic_finish (&m_dc);

    for (size_t i = 0; i < m_sinks.size (); i++)
      m_sinks[i] = nullptr;

    for (auto iter : m_str_to_file_map)
      delete iter.second;

    for (auto iter :m_location_t_map)
      delete iter.second;

    free (m_line_table.m_location_adhoc_data_map.data);
    free (m_line_table.info_ordinary.maps);
  }

  line_maps *get_line_table () { return &m_line_table; }
  diagnostics::context &get_dc () { return m_dc; }

  const diagnostics::logical_locations::manager &
  get_logical_location_manager () const
  {
    auto mgr = m_dc.get_logical_location_manager ();
    gcc_assert (mgr);
    return *mgr;
  }

  void write_patch (FILE *dst_stream);

  void add_sink (std::unique_ptr<sink> sink)
  {
    m_sinks.push_back (std::move (sink));
  }

  void emit_va (diagnostic &diag, const char *msgid, va_list *args)
    LIBGDIAGNOSTICS_PARAM_GCC_FORMAT_STRING(3, 0);

  void emit (diagnostic &diag, const char *msgid, ...)
    LIBGDIAGNOSTICS_PARAM_GCC_FORMAT_STRING(3, 4);

  void emit_msg_buf (diagnostic &diag,
		     diagnostic_message_buffer &msg_buf);

  diagnostic_file *
  new_file (const char *name,
	    const char *sarif_source_language)
  {
    if (diagnostic_file **slot = m_str_to_file_map.get (name))
      return *slot;
    diagnostic_file *file
      = new diagnostic_file (*this, name, sarif_source_language);
    m_str_to_file_map.put (file->get_name (), file);
    return file;
  }

  diagnostic_file *
  get_file_by_name (const char *name)
  {
    if (diagnostic_file **slot = m_str_to_file_map.get (name))
      return *slot;
    return nullptr;
  }

  const diagnostic_physical_location *
  new_location_from_file_and_line (const diagnostic_file *file,
				   diagnostic_line_num_t line_num)
  {
    if (m_debug_physical_locations)
      fprintf (stderr, "new_location_from_file_and_line (%s, %i)",
	       file->get_name (), line_num);
    ensure_linemap_for_file_and_line (file, line_num);
    location_t loc = linemap_position_for_column (&m_line_table, 0);
    return new_location (loc);
  }

  const diagnostic_physical_location *
  new_location_from_file_line_column (const diagnostic_file *file,
				      diagnostic_line_num_t line_num,
				      diagnostic_column_num_t column_num)
  {
    if (m_debug_physical_locations)
      fprintf (stderr, "new_location_from_file_line_column (%s, %i, %i)",
	       file->get_name (), line_num, column_num);
    ensure_linemap_for_file_and_line (file, line_num);
    location_t loc = linemap_position_for_column (&m_line_table, column_num);
    return new_location (loc);
  }

  const diagnostic_physical_location *
  new_location_from_range (const diagnostic_physical_location *loc_caret,
			   const diagnostic_physical_location *loc_start,
			   const diagnostic_physical_location *loc_end)
  {
    if (m_debug_physical_locations)
      fprintf (stderr, "new_location_from_range (%p, %p, %p)",
	       (const void *)loc_caret,
	       (const void *)loc_start,
	       (const void *)loc_end);
    return new_location
      (m_line_table.make_location (as_location_t (loc_caret),
				   as_location_t (loc_start),
				   as_location_t (loc_end)));
  }

  void
  set_debug_physical_locations (bool value)
  {
    m_debug_physical_locations = value;
  }

  const diagnostic_logical_location *
  new_logical_location (enum diagnostic_logical_location_kind_t kind,
			const diagnostic_logical_location *parent,
			const char *short_name,
			const char *fully_qualified_name,
			const char *decorated_name)
  {
    /* Use m_logical_locs to "uniquify" instances.  */
    diagnostic_logical_location key (kind,
				     parent,
				     short_name,
				     fully_qualified_name,
				     decorated_name);
    auto iter = m_logical_locs.find (key);
    if (iter != m_logical_locs.end ())
      return (*iter).second.get ();

    std::unique_ptr<diagnostic_logical_location> logical_loc
      = std::make_unique<diagnostic_logical_location> (kind,
						       parent,
						       short_name,
						       fully_qualified_name,
						       decorated_name);
    const diagnostic_logical_location *result = logical_loc.get ();
    m_logical_locs.insert
      (logical_locs_map_t::value_type (std::move (key),
				       std::move (logical_loc)));
    return result;
  }

  diagnostic_execution_path *
  new_execution_path ();

  void begin_group ()
  {
    m_dc.begin_group ();
  }

  void end_group ()
  {
    m_dc.end_group ();
  }

  const char *
  maybe_get_sarif_source_language (const char *filename)
  {
    if (diagnostic_file **slot = m_str_to_file_map.get (filename))
      {
	gcc_assert (*slot);
	return (*slot)->get_sarif_source_language ();
      }
    return nullptr;
  }

  const diagnostic *get_current_diag () { return m_current_diag; }

  const diagnostics::client_version_info *
  get_client_version_info () const
  {
    return &m_client_version_info;
  }
  impl_client_version_info *get_client_version_info ()
  {
    return &m_client_version_info;
  }

  void
  assert_valid_diagnostic_physical_location (const diagnostic_physical_location *loc) const
  {
    if (!loc)
      return;
    gcc_assert (loc->m_mgr == this);
  }

  /* TODO: Various things still use the "line_table" global variable.
     Set it to be this diagnostic_manager's m_line_table.
     Ideally we should eliminate this global (and this function).  */
  void set_line_table_global () const
  {
    line_table = const_cast<line_maps *> (&m_line_table);
  }

  const diagnostic_logical_location *
  get_prev_diag_logical_loc () const
  {
    return m_prev_diag_logical_loc;
  }

  void
  take_global_graph (std::unique_ptr<diagnostic_graph> graph);

private:
  void
  ensure_linemap_for_file_and_line (const diagnostic_file *file,
				    diagnostic_line_num_t linenum)
  {
    /* Build a simple linemap describing some locations. */
    if (LINEMAPS_ORDINARY_USED (&m_line_table) == 0)
      linemap_add (&m_line_table, LC_ENTER, false, file->get_name (), 0);
    else
      {
	line_map_ordinary *last_map
	  = LINEMAPS_LAST_ORDINARY_MAP (&m_line_table);
	if (last_map->to_file != file->get_name ()
	    || linenum < last_map->to_line)
	  {
	    line_map *map
	      = const_cast<line_map *>
	      (linemap_add (&m_line_table, LC_RENAME_VERBATIM, false,
			    file->get_name (), 0));
	    ((line_map_ordinary *)map)->included_from = UNKNOWN_LOCATION;
	  }
      }
    linemap_line_start (&m_line_table, linenum, 100);
  }

  const diagnostic_physical_location *
  new_location (location_t loc)
  {
    if (loc == UNKNOWN_LOCATION)
      return nullptr;
    if (m_debug_physical_locations)
      fprintf (stderr, ": new_location (%lx)", loc);
    if (diagnostic_physical_location **slot = m_location_t_map.get (loc))
      {
	if (m_debug_physical_locations)
	  fprintf (stderr, ": cache hit: %p\n", (const void *)*slot);
	return *slot;
      }
    diagnostic_physical_location *phys_loc
      = new diagnostic_physical_location (this, loc);
    m_location_t_map.put (loc, phys_loc);
    if (m_debug_physical_locations)
      fprintf (stderr, ": cache miss: %p\n", (const void *)phys_loc);
    return phys_loc;
  }

  diagnostics::context m_dc;
  line_maps m_line_table;
  impl_client_version_info m_client_version_info;
  std::vector<std::unique_ptr<sink>> m_sinks;
  hash_map<nofree_string_hash, diagnostic_file *> m_str_to_file_map;
  hash_map<int_hash<location_t, UNKNOWN_LOCATION, location_t (-1)>,
	   diagnostic_physical_location *> m_location_t_map;
  typedef std::map<diagnostic_logical_location,
		   std::unique_ptr<diagnostic_logical_location>> logical_locs_map_t;
  logical_locs_map_t m_logical_locs;
  const diagnostic *m_current_diag;
  const diagnostic_logical_location *m_prev_diag_logical_loc;
  std::unique_ptr<diagnostics::changes::change_set> m_change_set;
  bool m_debug_physical_locations;
};

class impl_rich_location : public rich_location
{
public:
  impl_rich_location (line_maps *set)
  : rich_location (set, UNKNOWN_LOCATION)
  {}
};

class impl_range_label : public range_label
{
public:
  impl_range_label (const char *text)
  : m_text (xstrdup (text))
  {}

  ~impl_range_label () { free (m_text); }

  label_text get_text (unsigned) const final override
  {
    return label_text::borrow (m_text);
  }

private:
  char *m_text;
};

class impl_rule : public diagnostics::metadata::rule
{
public:
  impl_rule (const char *title, const char *url)
  :  m_title (title),
     m_url (url)
  {
  }

  virtual ~impl_rule () {}

  char *make_description () const final override
  {
    return m_title.xstrdup ();
  }

  char *make_url () const final override
  {
    return m_url.xstrdup ();
  }

private:
  owned_nullable_string m_title;
  owned_nullable_string m_url;
};

struct diagnostic_graph : public diagnostics::digraphs::digraph
{
  diagnostic_graph (diagnostic_manager &) {}

  diagnostic_node *
  add_node_with_id (std::string node_id,
		    diagnostic_node *parent_node);
  diagnostic_edge *
  add_edge_with_label (const char *edge_id,
		       diagnostic_node &src_node,
		       diagnostic_node &dst_node,
		       const char *label);
};

struct diagnostic_node : public diagnostics::digraphs::node
{
  diagnostic_node (diagnostic_graph &g,
		   std::string id)
  : node (g, std::move (id))
  {
  }
};

struct diagnostic_edge : public diagnostics::digraphs::edge
{
  diagnostic_edge (diagnostic_graph &g,
		   const char *id,
		   diagnostic_node &src_node,
		   diagnostic_node &dst_node)
  : edge (g, id, src_node, dst_node)
  {
  }
};

class libgdiagnostics_path_event : public diagnostics::paths::event
{
public:
  libgdiagnostics_path_event (const diagnostic_physical_location *physical_loc,
			      const diagnostic_logical_location *logical_loc,
			      unsigned stack_depth,
			      std::unique_ptr<diagnostic_graph> state_graph,
			      std::unique_ptr<diagnostic_message_buffer> msg_buf)
  : m_physical_loc (physical_loc),
    m_logical_loc (logical_loc),
    m_stack_depth (stack_depth),
    m_state_graph (std::move (state_graph)),
    m_msg_buf (std::move (msg_buf))
  {
    gcc_assert (m_msg_buf);
  }

  /* diagnostics::paths::event vfunc implementations.  */

  location_t get_location () const final override
  {
    return as_location_t (m_physical_loc);
  }

  int get_stack_depth () const final override
  {
    return m_stack_depth;
  }

  void print_desc (pretty_printer &pp) const final override
  {
    if (m_msg_buf)
      {
	pp_element_message_buffer e_msg_buf (*m_msg_buf);
	pp_printf (&pp, "%e", &e_msg_buf);
      }
  }

  diagnostics::logical_locations::key
  get_logical_location () const final override
  {
    return impl_logical_location_manager::key_from_ptr (m_logical_loc);
  }

  meaning get_meaning () const final override
  {
    return meaning ();
  }

  bool connect_to_next_event_p () const final override
  {
    return false; // TODO
  }

  diagnostics::paths::thread_id_t get_thread_id () const final override
  {
    return 0;
  }

  std::unique_ptr<diagnostics::digraphs::digraph>
  maybe_make_diagnostic_state_graph (bool) const final override
  {
    if (!m_state_graph)
      return nullptr;

    return m_state_graph->clone ();
  }

private:
  static label_text make_desc (const char *gmsgid,
			       va_list *args,
			       bool colorize)
  {
    va_list copy_of_args;
    va_copy (copy_of_args, *args);

    // TODO: when should localization happen?
    text_info text (gmsgid, &copy_of_args, errno);
    pretty_printer pp;
    pp_show_color (&pp) = colorize;
    pp.set_output_stream (nullptr);
    pp_format (&pp, &text);
    pp_output_formatted_text (&pp, nullptr);
    label_text result = label_text::take (xstrdup (pp_formatted_text (&pp)));

    va_end (copy_of_args);

    return result;
  }

  const diagnostic_physical_location *m_physical_loc;
  const diagnostic_logical_location *m_logical_loc;
  unsigned m_stack_depth;
  std::unique_ptr<diagnostic_graph> m_state_graph;
  std::unique_ptr<diagnostic_message_buffer> m_msg_buf;
};

class libgdiagnostics_path_thread : public diagnostics::paths::thread
{
public:
  libgdiagnostics_path_thread (const char *name) : m_name (name) {}
  label_text get_name (bool) const final override
  {
    return label_text::borrow (m_name);
  }

private:
  const char *m_name; // has been i18n-ed and formatted
};

/* This has to be a "struct" as it is exposed in the C API.  */

struct diagnostic_execution_path : public diagnostics::paths::path
{
  diagnostic_execution_path (const diagnostics::logical_locations::manager &logical_loc_mgr)
  : diagnostics::paths::path (logical_loc_mgr),
    m_thread ("")
  {
  }

  diagnostics::paths::event_id_t
  add_event_va (const diagnostic_physical_location *physical_loc,
		const diagnostic_logical_location *logical_loc,
		unsigned stack_depth,
		std::unique_ptr<diagnostic_graph> state_graph,
		const char *gmsgid,
		va_list *args)
  {
    auto msg_buf = std::make_unique<diagnostic_message_buffer> (gmsgid, args);

    m_events.push_back
      (std::make_unique<libgdiagnostics_path_event> (physical_loc,
						     logical_loc,
						     stack_depth,
						     std::move (state_graph),
						     std::move (msg_buf)));
    return m_events.size () - 1;
  }

  diagnostic_event_id_t
  add_event_via_msg_buf (const diagnostic_physical_location *physical_loc,
			 const diagnostic_logical_location *logical_loc,
			 unsigned stack_depth,
			 std::unique_ptr<diagnostic_graph> state_graph,
			 std::unique_ptr<diagnostic_message_buffer> msg_buf)
  {
    m_events.push_back
      (std::make_unique<libgdiagnostics_path_event> (physical_loc,
						     logical_loc,
						     stack_depth,
						     std::move (state_graph),
						     std::move (msg_buf)));
    return m_events.size () - 1;
  }

  /* diagnostics::paths::path vfunc implementations.  */

  unsigned num_events () const final override
  {
    return m_events.size ();
  }
  const diagnostics::paths::event & get_event (int idx) const final override
  {
    return *m_events[idx];
  }
  unsigned num_threads () const final override { return 1; }
  const diagnostics::paths::thread &
  get_thread (diagnostics::paths::thread_id_t) const final override
  {
    return m_thread;
  }

  bool
  same_function_p (int event_idx_a,
		   int event_idx_b) const final override
  {
    using logical_location = diagnostics::logical_locations::key;
    logical_location logical_loc_a
      = m_events[event_idx_a]->get_logical_location ();
    logical_location  logical_loc_b
      = m_events[event_idx_b]->get_logical_location ();

    /* Pointer equality, as we uniqify logical location instances.  */
    return logical_loc_a == logical_loc_b;
  }

private:
  libgdiagnostics_path_thread m_thread;
  std::vector<std::unique_ptr<libgdiagnostics_path_event>> m_events;
};

class prebuilt_digraphs
  : public lazily_created<std::vector<std::unique_ptr<diagnostics::digraphs::digraph>>>
{
public:
  using digraph = diagnostics::digraphs::digraph;

  std::unique_ptr<std::vector<std::unique_ptr<digraph>>>
  create_object () const final override
  {
    return std::make_unique<std::vector<std::unique_ptr<digraph>>> (std::move (m_digraphs));
  }

  void
  take_graph (std::unique_ptr<diagnostic_graph> graph)
  {
    m_digraphs.push_back (std::move (graph));
  }

private:
  mutable std::vector<std::unique_ptr<digraph>> m_digraphs;
};

/* This has to be a "struct" as it is exposed in the C API.  */

struct diagnostic
{
public:
  diagnostic (diagnostic_manager &diag_mgr,
	      enum diagnostic_level level)
  : m_diag_mgr (diag_mgr),
    m_level (level),
    m_rich_loc (diag_mgr.get_line_table ()),
    m_logical_loc (nullptr),
    m_path (nullptr),
    m_nesting_level (0)
  {
    m_metadata.set_lazy_digraphs (&m_graphs);
  }

  diagnostic_manager &get_manager () const
  {
    return m_diag_mgr;
  }

  enum diagnostic_level get_level () const { return m_level; }

  rich_location *get_rich_location () { return &m_rich_loc; }
  const diagnostics::metadata *get_metadata () { return &m_metadata; }

  void set_cwe (unsigned cwe_id)
  {
    m_metadata.add_cwe (cwe_id);
  }

  void add_rule (const char *title,
		 const char *url)
  {
    std::unique_ptr<impl_rule> rule = std::make_unique<impl_rule> (title, url);
    m_metadata.add_rule (*rule.get ());
    m_rules.push_back (std::move (rule));
  }

  void set_location (const diagnostic_physical_location *loc)
  {
    m_rich_loc.set_range (0, as_location_t (loc), SHOW_RANGE_WITH_CARET);
  }

  void
  add_location (const diagnostic_physical_location *loc)
  {
    m_rich_loc.add_range (as_location_t (loc),
			  SHOW_RANGE_WITHOUT_CARET);
  }

  void
  add_location_with_label (const diagnostic_physical_location *loc,
			   const char *text)
  {
    std::unique_ptr<range_label> label
      = std::make_unique <impl_range_label> (text);
    m_rich_loc.add_range (as_location_t (loc),
			  SHOW_RANGE_WITHOUT_CARET,
			  label.get ());
    m_labels.push_back (std::move (label));
  }

  void
  add_location_with_label (const diagnostic_physical_location *loc,
			   std::unique_ptr<diagnostic_message_buffer> msg_buf)
  {
    std::string str = msg_buf->to_string ();
    std::unique_ptr<range_label> label
      = std::make_unique <impl_range_label> (str.c_str ());
    m_rich_loc.add_range (as_location_t (loc),
			  SHOW_RANGE_WITHOUT_CARET,
			  label.get ());
    m_labels.push_back (std::move (label));
  }

  void
  set_logical_location (const diagnostic_logical_location *logical_loc)
  {
    m_logical_loc = logical_loc;
  }
  const diagnostic_logical_location *get_logical_location () const
  {
    return m_logical_loc;
  }

  diagnostic_execution_path *
  add_execution_path ()
  {
    m_path
      = std::make_unique<diagnostic_execution_path>
	  (m_diag_mgr.get_logical_location_manager ());
    m_rich_loc.set_path (m_path.get ());
    return m_path.get ();
  }

  void
  take_execution_path (diagnostic_execution_path *path)
  {
    m_path = std::unique_ptr<diagnostic_execution_path> (path);
    m_rich_loc.set_path (path);
  }

  void
  take_graph (std::unique_ptr<diagnostic_graph> graph)
  {
    m_graphs.take_graph (std::move (graph));
  }

  const prebuilt_digraphs &
  get_graphs () const
  {
    return m_graphs;
  }

  int get_nesting_level () const { return m_nesting_level; }
  void set_nesting_level (int value) { m_nesting_level = value; }

private:
  diagnostic_manager &m_diag_mgr;
  enum diagnostic_level m_level;
  impl_rich_location m_rich_loc;
  const diagnostic_logical_location *m_logical_loc;
  diagnostics::metadata m_metadata;
  prebuilt_digraphs m_graphs;
  std::vector<std::unique_ptr<range_label>> m_labels;
  std::vector<std::unique_ptr<impl_rule>> m_rules;
  std::unique_ptr<diagnostic_execution_path> m_path;
  int m_nesting_level;
};

static enum diagnostics::kind
diagnostics_kind_from_diagnostic_level (enum diagnostic_level level)
{
  switch (level)
    {
    default:
      gcc_unreachable ();
    case DIAGNOSTIC_LEVEL_ERROR:
      return diagnostics::kind::error;
    case DIAGNOSTIC_LEVEL_WARNING:
      return diagnostics::kind::warning;
    case DIAGNOSTIC_LEVEL_NOTE:
      return diagnostics::kind::note;
    case DIAGNOSTIC_LEVEL_SORRY:
      return diagnostics::kind::sorry;
    }
}

void
diagnostic_file::set_buffered_content (const char *buf, size_t sz)
{
  m_content = std::make_unique<content_buffer> (buf, sz);

  // Populate file_cache:
  diagnostics::file_cache &fc = m_mgr.get_dc ().get_file_cache ();
  fc.add_buffered_content (m_name.get_str (), buf, sz);
}

// struct diagnostic_physical_location

diagnostic_file *
diagnostic_physical_location::get_file () const
{
  m_mgr->set_line_table_global ();
  const char *filename = LOCATION_FILE (m_inner);
  if (!filename)
    return nullptr;
  return m_mgr->get_file_by_name (filename);
}

/* class impl_diagnostic_client_data_hooks.  */

const diagnostics::client_version_info *
impl_diagnostic_client_data_hooks::get_any_version_info () const
{
  return m_mgr.get_client_version_info ();
}

diagnostics::logical_locations::key
impl_diagnostic_client_data_hooks::get_current_logical_location () const
{
  gcc_assert (m_mgr.get_current_diag ());

  return impl_logical_location_manager::key_from_ptr
    (m_mgr.get_current_diag ()->get_logical_location ());
}

const char *
impl_diagnostic_client_data_hooks::
maybe_get_sarif_source_language (const char *filename) const
{
  return m_mgr.maybe_get_sarif_source_language (filename);
}

void
impl_diagnostic_client_data_hooks::
add_sarif_invocation_properties (diagnostics::sarif_object &) const
{
  // No-op.
}

/* struct diagnostic_text_sink : public sink.  */

diagnostic_text_sink::diagnostic_text_sink (diagnostic_manager &mgr,
					    FILE *dst_stream,
					    enum diagnostic_colorize colorize)
: sink (mgr),
  m_source_printing (mgr.get_dc ().get_source_printing_options ())
{
  auto inner_sink
    = std::make_unique<diagnostics::text_sink> (mgr.get_dc (),
						       &m_source_printing);
  inner_sink->get_printer ()->set_output_stream (dst_stream);
  m_inner_sink = inner_sink.get ();
  set_colorize (colorize);
  mgr.get_dc ().add_sink (std::move (inner_sink));
}

void
diagnostic_text_sink::set_colorize (enum diagnostic_colorize colorize)
{
  pretty_printer *const pp = m_inner_sink->get_printer ();
  switch (colorize)
    {
    default:
      gcc_unreachable ();
    case DIAGNOSTIC_COLORIZE_IF_TTY:
      pp_show_color (pp)
	= pp_show_color (m_mgr.get_dc ().get_reference_printer ());
      break;
    case DIAGNOSTIC_COLORIZE_NO:
      pp_show_color (pp) = false;
      break;
    case DIAGNOSTIC_COLORIZE_YES:
      pp_show_color (pp) = true;
      break;
    }
}

void
diagnostic_text_sink::text_starter (diagnostics::text_sink &text_output,
				    const diagnostics::diagnostic_info *info)
{
  gcc_assert (info->m_x_data);
  const diagnostic &diag = *static_cast<const diagnostic *> (info->m_x_data);
  pretty_printer *pp = text_output.get_printer ();
  const diagnostic_logical_location *diag_logical_loc
    = diag.get_logical_location ();
  diagnostic_manager &mgr = diag.get_manager ();
  if (diag_logical_loc && diag_logical_loc != mgr.get_prev_diag_logical_loc ())
    {
      pp_set_prefix (pp, nullptr);

      /* This macro is used to ensure that all format strings are visible to gettext
	 and checked at compile time.  */

#define CASE(KIND, MSGID) \
	case KIND:							\
	  if (const char *name						\
	      = diag_logical_loc->m_fully_qualified_name.get_str ())	\
	    {								\
	     pp_printf (pp, (MSGID), name);				\
	     pp_character (pp, ':');					\
	     pp_newline (pp);						\
	    }								\
	  break;

      switch (diag_logical_loc->m_kind)
	{
	default:
	  break;

	/* Kinds within executable code.  */
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION, _("In function %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_MEMBER, _("In member %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_MODULE, _("In module %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_NAMESPACE, _("In namespace %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_TYPE, _("In type %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_RETURN_TYPE,
	     _("In return type %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_PARAMETER, _("In parameter %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_VARIABLE, _("In variable %qs"))

	/* Kinds within XML or HTML documents.  */
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_ELEMENT, _("In element %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_ATTRIBUTE, _("In attribute %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_TEXT, _("In text %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_COMMENT, _("In comment %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_PROCESSING_INSTRUCTION,
	     _("In processing instruction %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_DTD, _("In DTD %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_DECLARATION,
	     _("In declaration %qs"))

	/* Kinds within JSON documents.  */
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_OBJECT, _("In JSON object %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_ARRAY, _("In JSON array %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_PROPERTY,
	     _("In JSON property %qs"))
	CASE(DIAGNOSTIC_LOGICAL_LOCATION_KIND_VALUE, _("In JSON value %qs"))
	}

#undef CASE
    }
  pp_set_prefix (pp,
		 text_output.build_prefix (*info));
}

/* class sarif_sink : public sink.  */

sarif_sink::
sarif_sink (diagnostic_manager &mgr,
	    FILE *dst_stream,
	    const diagnostic_file *main_input_file,
	    const diagnostics::sarif_generation_options &sarif_gen_opts)
: sink (mgr)
{
  diagnostics::output_file output_file (dst_stream, false,
					label_text::borrow ("sarif_sink"));
  auto serialization
    = std::make_unique<diagnostics::sarif_serialization_format_json> (true);
  auto inner_sink = make_sarif_sink (mgr.get_dc (),
				     *mgr.get_line_table (),
				     std::move (serialization),
				     sarif_gen_opts,
				     std::move (output_file));
  inner_sink->set_main_input_filename (main_input_file->get_name ());
  mgr.get_dc ().add_sink (std::move (inner_sink));
}

// struct diagnostic_message_buffer

std::string
diagnostic_message_buffer::to_string () const
{
  std::string result;

  /* Convert to text, dropping colorization, URLs, etc.  */
  for (auto iter = m_tokens.m_first; iter; iter = iter->m_next)
    switch (iter->m_kind)
      {
      default:
	gcc_unreachable ();

      case pp_token::kind::text:
	{
	  pp_token_text *sub = as_a <pp_token_text *> (iter);
	  result += sub->m_value.get ();
	}
	break;

      case pp_token::kind::begin_color:
      case pp_token::kind::end_color:
	// Skip
	break;

      case pp_token::kind::begin_quote:
	result += open_quote;
	break;

      case pp_token::kind::end_quote:
	result += close_quote;
	break;

      case pp_token::kind::begin_url:
      case pp_token::kind::end_url:
	// Skip
	break;

      case pp_token::kind::event_id:
	{
	  pp_token_event_id *sub = as_a <pp_token_event_id *> (iter);
	  gcc_assert (sub->m_event_id.known_p ());
	  result += '(';
	  result += std::to_string (sub->m_event_id.one_based ());
	  result += ')';
	}
	break;

      case pp_token::kind::custom_data:
	/* We don't have a way of handling custom_data tokens here.  */
	gcc_unreachable ();
	break;
      }

  return result;
}

/* struct diagnostic_manager.  */

void
diagnostic_manager::write_patch (FILE *dst_stream)
{
  pretty_printer pp;
  pp.set_output_stream (dst_stream);
  m_change_set->print_diff (&pp, true);
  pp_flush (&pp);
}

void
diagnostic_manager::emit_va (diagnostic &diag, const char *msgid, va_list *args)
{
  set_line_table_global ();

  m_current_diag = &diag;

  {
    m_dc.begin_group ();

    diagnostics::diagnostic_info info;
GCC_DIAGNOSTIC_PUSH_IGNORED(-Wsuggest-attribute=format)
    diagnostic_set_info (&info, msgid, args, diag.get_rich_location (),
			 diagnostics_kind_from_diagnostic_level
			   (diag.get_level ()));
GCC_DIAGNOSTIC_POP
    info.m_metadata = diag.get_metadata ();
    info.m_x_data = &diag;
    m_dc.set_nesting_level (diag.get_nesting_level ());
    diagnostic_report_diagnostic (&m_dc, &info);
    m_dc.set_nesting_level (0);
    m_dc.end_group ();
  }

  rich_location *rich_loc = diag.get_rich_location ();
  if (rich_loc->fixits_can_be_auto_applied_p ())
    m_change_set->add_fixits (rich_loc);

  m_prev_diag_logical_loc = diag.get_logical_location ();
  m_current_diag = nullptr;
}

void
diagnostic_manager::emit (diagnostic &diag, const char *msgid, ...)
{
  va_list args;
  va_start (args, msgid);
  emit_va (diag, msgid, &args);
  va_end (args);
}

void
diagnostic_manager::emit_msg_buf (diagnostic &diag,
				  diagnostic_message_buffer &msg_buf)
{

  pp_element_message_buffer e_msg_buf (msg_buf);
  emit (diag, "%e", &e_msg_buf);
}

diagnostic_execution_path *
diagnostic_manager::new_execution_path ()
{
  auto mgr = m_dc.get_logical_location_manager ();
  gcc_assert (mgr);
  return new diagnostic_execution_path (*mgr);
}

void
diagnostic_manager::take_global_graph (std::unique_ptr<diagnostic_graph> graph)
{
  class prebuilt_lazy_digraph : public lazily_created<diagnostics::digraphs::digraph>
  {
  public:
    prebuilt_lazy_digraph (std::unique_ptr<diagnostic_graph> graph)
      : m_graph (std::move (graph))
    {
    }

    std::unique_ptr<diagnostics::digraphs::digraph>
    create_object () const final override
    {
      return std::move (m_graph);
    }

  private:
    mutable std::unique_ptr<diagnostic_graph> m_graph;
  };

  m_dc.report_global_digraph (prebuilt_lazy_digraph (std::move (graph)));
}
/* Error-checking at the API boundary.  */

#define FAIL_IF_NULL(PTR_ARG) \
  do {							    \
    volatile const void *ptr_arg = (PTR_ARG);		    \
    if (!ptr_arg) {					    \
      fprintf (stderr, "%s: %s must be non-NULL\n",	    \
	       __func__, #PTR_ARG);		      \
      abort ();					      \
    }						    \
  } while (0)

/* Public entrypoints.  */

/* Public entrypoint for clients to acquire a diagnostic_manager.  */

diagnostic_manager *
diagnostic_manager_new (void)
{
  return new diagnostic_manager ();
}

/* Public entrypoint for clients to release a diagnostic_manager.  */

void
diagnostic_manager_release (diagnostic_manager *diag_mgr)
{
  delete diag_mgr;
}

/* Public entrypoint.  */

void
diagnostic_manager_set_tool_name (diagnostic_manager *diag_mgr,
				  const char *value)
{
  FAIL_IF_NULL (diag_mgr);
  FAIL_IF_NULL (value);

  diag_mgr->get_client_version_info ()->m_name.set (value);
}

/* Public entrypoint.  */

void
diagnostic_manager_set_full_name (diagnostic_manager *diag_mgr,
				  const char *value)
{
  FAIL_IF_NULL (diag_mgr);
  FAIL_IF_NULL (value);

  diag_mgr->get_client_version_info ()->m_full_name.set (value);
}

/* Public entrypoint.  */

void
diagnostic_manager_set_version_string (diagnostic_manager *diag_mgr,
				       const char *value)
{
  FAIL_IF_NULL (diag_mgr);
  FAIL_IF_NULL (value);

  diag_mgr->get_client_version_info ()->m_version.set (value);
}

/* Public entrypoint.  */

void
diagnostic_manager_set_version_url (diagnostic_manager *diag_mgr,
				    const char *value)
{
  FAIL_IF_NULL (diag_mgr);
  FAIL_IF_NULL (value);

  diag_mgr->get_client_version_info ()->m_version_url.set (value);
}

/* Public entrypoint.  */

diagnostic_text_sink *
diagnostic_manager_add_text_sink (diagnostic_manager *diag_mgr,
				  FILE *dst_stream,
				  enum diagnostic_colorize colorize)
{
  FAIL_IF_NULL (diag_mgr);
  FAIL_IF_NULL (dst_stream);

  diagnostic_text_sink *result
    = new diagnostic_text_sink (*diag_mgr, dst_stream, colorize);
  diag_mgr->add_sink (std::unique_ptr<sink> (result));
  return result;
}

/* Public entrypoint.  */

void
diagnostic_text_sink_set_source_printing_enabled (diagnostic_text_sink *text_sink,
						  int value)
{
  FAIL_IF_NULL (text_sink);

  text_sink->get_source_printing_options ().enabled = value;
}

/* Public entrypoint.  */

void
diagnostic_text_sink_set_colorize (diagnostic_text_sink *text_sink,
				   enum diagnostic_colorize colorize)
{
  FAIL_IF_NULL (text_sink);

  text_sink->set_colorize (colorize);
}

/* Public entrypoint.  */

void
diagnostic_text_sink_set_labelled_source_colorization_enabled (diagnostic_text_sink *text_sink,
							       int value)
{
  FAIL_IF_NULL (text_sink);

  text_sink->get_source_printing_options ().colorize_source_p = value;
}


/* Public entrypoint.  */

void
diagnostic_manager_add_sarif_sink (diagnostic_manager *diag_mgr,
				   FILE *dst_stream,
				   const diagnostic_file *main_input_file,
				   enum diagnostic_sarif_version version)
{
  FAIL_IF_NULL (diag_mgr);
  FAIL_IF_NULL (dst_stream);
  FAIL_IF_NULL (main_input_file);

  diagnostics::sarif_generation_options sarif_gen_opts;
  switch (version)
    {
    default:
      fprintf (stderr, "%s: unrecognized value for version: %i\n",
	       __func__, (int)version);
      abort ();
    case DIAGNOSTIC_SARIF_VERSION_2_1_0:
      sarif_gen_opts.m_version = diagnostics::sarif_version::v2_1_0;
      break;
    case DIAGNOSTIC_SARIF_VERSION_2_2_PRERELEASE:
      sarif_gen_opts.m_version
	= diagnostics::sarif_version::v2_2_prerelease_2024_08_08;
      break;
    }

  diag_mgr->add_sink (std::make_unique<sarif_sink> (*diag_mgr,
						    dst_stream,
						    main_input_file,
						    sarif_gen_opts));
}

/* Public entrypoint.  */

void
diagnostic_manager_write_patch (diagnostic_manager *diag_mgr,
				FILE *dst_stream)
{
  FAIL_IF_NULL (diag_mgr);
  FAIL_IF_NULL (dst_stream);

  diag_mgr->write_patch (dst_stream);
}

/* Public entrypoint.  */

diagnostic_file *
diagnostic_manager_new_file (diagnostic_manager *diag_mgr,
			     const char *name,
			     const char *sarif_source_language)
{
  FAIL_IF_NULL (diag_mgr);
  FAIL_IF_NULL (name);

  return diag_mgr->new_file (name, sarif_source_language);
}

/* Public entrypoint.  */

void
diagnostic_file_set_buffered_content (diagnostic_file *file,
				      const char *buf,
				      size_t sz)
{
  FAIL_IF_NULL (file);
  FAIL_IF_NULL (buf);

  file->set_buffered_content (buf, sz);
}

void
diagnostic_manager_debug_dump_file (diagnostic_manager *,
				    const diagnostic_file *file,
				    FILE *out)
{
  FAIL_IF_NULL (out);
  if (file)
    {
      fprintf (out, "file(name=\"%s\"",
	       file->get_name ());
      if (file->get_sarif_source_language ())
	fprintf (out, ", sarif_source_language=\"%s\"",
		 file->get_sarif_source_language ());
      if (const content_buffer *buf = file->get_content ())
	fprintf (out, ", content=(size=" HOST_SIZE_T_PRINT_DEC ")",
		 buf->m_sz);
      fprintf (out, ")");
    }
  else
    fprintf (out, "(null)");
}


/* Public entrypoint.  */

const diagnostic_physical_location *
diagnostic_manager_new_location_from_file_and_line (diagnostic_manager *diag_mgr,
						    const diagnostic_file *file,
						    diagnostic_line_num_t linenum)
{
  FAIL_IF_NULL (diag_mgr);
  FAIL_IF_NULL (file);

  return diag_mgr->new_location_from_file_and_line (file, linenum);
}

/* Public entrypoint.  */

const diagnostic_physical_location *
diagnostic_manager_new_location_from_file_line_column (diagnostic_manager *diag_mgr,
						       const diagnostic_file *file,
						       diagnostic_line_num_t line_num,
					       diagnostic_column_num_t column_num)
{
  FAIL_IF_NULL (diag_mgr);
  FAIL_IF_NULL (file);

  return diag_mgr->new_location_from_file_line_column (file,
						       line_num,
						       column_num);
}

/* Public entrypoint.  */

const diagnostic_physical_location *
diagnostic_manager_new_location_from_range (diagnostic_manager *diag_mgr,
					    const diagnostic_physical_location *loc_caret,
					    const diagnostic_physical_location *loc_start,
					    const diagnostic_physical_location *loc_end)
{
  FAIL_IF_NULL (diag_mgr);

  return diag_mgr->new_location_from_range (loc_caret,
					    loc_start,
					    loc_end);
}

/* Public entrypoint.  */

void
diagnostic_manager_debug_dump_location (const diagnostic_manager *diag_mgr,
					const diagnostic_physical_location *loc,
					FILE *out)
{
  FAIL_IF_NULL (diag_mgr);
  FAIL_IF_NULL (out);

  if (loc)
    {
      const location_t cpplib_loc = as_location_t (loc);
      diag_mgr->set_line_table_global ();
      const expanded_location exp_loc (expand_location (cpplib_loc));

      diagnostics::context dc;
      diagnostic_initialize (&dc, 0);
      dc.m_show_column = true;

      diagnostics::text_sink text_output (dc);
      label_text loc_text = text_output.get_location_text (exp_loc);
      fprintf (out, "%s", loc_text.get ());

      diagnostic_finish (&dc);
    }
  else
    fprintf (out, "(null)");
}

/* Public entrypoint.  */

const diagnostic_logical_location *
diagnostic_manager_new_logical_location (diagnostic_manager *diag_mgr,
					 enum diagnostic_logical_location_kind_t kind,
					 const diagnostic_logical_location *parent,
					 const char *short_name,
					 const char *fully_qualified_name,
					 const char *decorated_name)
{
  FAIL_IF_NULL (diag_mgr);

  return diag_mgr->new_logical_location (kind,
					 parent,
					 short_name,
					 fully_qualified_name,
					 decorated_name);
}

void
diagnostic_manager_debug_dump_logical_location (const diagnostic_manager *diag_mgr,
						const diagnostic_logical_location *loc,
						FILE *out)
{
  FAIL_IF_NULL (diag_mgr);
  FAIL_IF_NULL (out);

  if (loc)
    {
      fprintf (out, "logical_location(kind=");
      switch (loc->m_kind)
	{
	default:
	  gcc_unreachable ();

	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION:
	  fprintf (out, "function");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_MEMBER:
	  fprintf (out, "member");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_MODULE:
	  fprintf (out, "module");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_NAMESPACE:
	  fprintf (out, "namespace");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_TYPE:
	  fprintf (out, "file");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_RETURN_TYPE:
	  fprintf (out, "return_type");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_PARAMETER:
	  fprintf (out, "parameter");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_VARIABLE:
	  fprintf (out, "variable");
	  break;

	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_ELEMENT:
	  fprintf (out, "element");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_ATTRIBUTE:
	  fprintf (out, "attribute");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_TEXT:
	  fprintf (out, "text");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_COMMENT:
	  fprintf (out, "comment");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_PROCESSING_INSTRUCTION:
	  fprintf (out, "processing_instruction");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_DTD:
	  fprintf (out, "dtd");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_DECLARATION:
	  fprintf (out, "declaration");
	  break;

	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_OBJECT:
	  fprintf (out, "object");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_ARRAY:
	  fprintf (out, "array");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_PROPERTY:
	  fprintf (out, "property");
	  break;
	case DIAGNOSTIC_LOGICAL_LOCATION_KIND_VALUE:
	  fprintf (out, "value");
	  break;
	}
      if (auto parent = loc->m_parent)
	{
         fprintf (out, ", parent=");
	 diagnostic_manager_debug_dump_logical_location (diag_mgr,
							 parent,
							 out);
	}
      if (const char *val = loc->m_short_name.get_str ())
	fprintf (out, ", short_name=\"%s\"", val);
      if (const char *val = loc->m_fully_qualified_name.get_str ())
	fprintf (out, ", fully_qualified_name=\"%s\"", val);
      if (const char *val = loc->m_decorated_name.get_str ())
	fprintf (out, ", decorated_name=\"%s\"", val);
      fprintf (out, ")");
    }
  else
    fprintf (out, "(null)");
}

/* Public entrypoint.  */

void
diagnostic_manager_begin_group (diagnostic_manager *diag_mgr)
{
  FAIL_IF_NULL (diag_mgr);
  diag_mgr->begin_group ();
}

/* Public entrypoint.  */

extern void
diagnostic_manager_end_group (diagnostic_manager *diag_mgr)
{
  FAIL_IF_NULL (diag_mgr);
  diag_mgr->end_group ();
}

/* Public entrypoint.  */

diagnostic *
diagnostic_begin (diagnostic_manager *diag_mgr,
		  enum diagnostic_level level)
{
  FAIL_IF_NULL (diag_mgr);

  return new diagnostic (*diag_mgr, level);
}

/* Public entrypoint.  */

void
diagnostic_set_cwe (diagnostic *diag,
		    unsigned cwe_id)
{
  FAIL_IF_NULL (diag);

  diag->set_cwe (cwe_id);
}

/* Public entrypoint.  */

void
diagnostic_add_rule (diagnostic *diag,
		     const char *title,
		     const char *url)
{
  FAIL_IF_NULL (diag);

  diag->add_rule (title, url);
}

/* Public entrypoint.  */

void
diagnostic_set_location (diagnostic *diag,
			 const diagnostic_physical_location *loc)
{
  FAIL_IF_NULL (diag);
  diag->get_manager ().assert_valid_diagnostic_physical_location (loc);

  diag->set_location (loc);
}

/* Public entrypoint.  */

void
diagnostic_add_location (diagnostic *diag,
			 const diagnostic_physical_location *loc)
{
  FAIL_IF_NULL (diag);
  diag->get_manager ().assert_valid_diagnostic_physical_location (loc);

  diag->add_location (loc);
}

/* Public entrypoint.  */

void
diagnostic_add_location_with_label (diagnostic *diag,
				    const diagnostic_physical_location *loc,
				    const char *text)
{
  FAIL_IF_NULL (diag);
  diag->get_manager ().assert_valid_diagnostic_physical_location (loc);
  FAIL_IF_NULL (text);

  diag->add_location_with_label (loc, text);
}

/* Public entrypoint.  */

void
diagnostic_set_logical_location (diagnostic *diag,
				 const diagnostic_logical_location *logical_loc)
{
  FAIL_IF_NULL (diag);

  diag->set_logical_location (logical_loc);
}

/* Public entrypoint.  */

void
diagnostic_add_fix_it_hint_insert_before (diagnostic *diag,
					  const diagnostic_physical_location *loc,
					  const char *addition)
{
  FAIL_IF_NULL (diag);
  diag->get_manager ().assert_valid_diagnostic_physical_location (loc);
  FAIL_IF_NULL (addition);

  diag->get_manager ().set_line_table_global ();
  diag->get_rich_location ()->add_fixit_insert_before (as_location_t (loc),
						       addition);
}

/* Public entrypoint.  */

void
diagnostic_add_fix_it_hint_insert_after (diagnostic *diag,
					 const diagnostic_physical_location *loc,
					 const char *addition)
{
  FAIL_IF_NULL (diag);
  diag->get_manager ().assert_valid_diagnostic_physical_location (loc);
  FAIL_IF_NULL (addition);

  diag->get_manager ().set_line_table_global ();
  diag->get_rich_location ()->add_fixit_insert_after (as_location_t (loc),
						      addition);
}

/* Public entrypoint.  */

void
diagnostic_add_fix_it_hint_replace (diagnostic *diag,
				    const diagnostic_physical_location *loc,
				    const char *replacement)
{
  FAIL_IF_NULL (diag);
  diag->get_manager ().assert_valid_diagnostic_physical_location (loc);
  FAIL_IF_NULL (replacement);

  diag->get_manager ().set_line_table_global ();
  diag->get_rich_location ()->add_fixit_replace (as_location_t (loc),
						 replacement);
}

/* Public entrypoint.  */

void
diagnostic_add_fix_it_hint_delete (diagnostic *diag,
				   const diagnostic_physical_location *loc)
{
  FAIL_IF_NULL (diag);
  diag->get_manager ().assert_valid_diagnostic_physical_location (loc);

  diag->get_manager ().set_line_table_global ();
  diag->get_rich_location ()->add_fixit_remove (as_location_t (loc));
}

/* Public entrypoint.  */

diagnostic_execution_path *
diagnostic_add_execution_path (diagnostic *diag)
{
  FAIL_IF_NULL (diag);

  return diag->add_execution_path ();
}

/* Public entrypoint.  */

diagnostic_execution_path *
diagnostic_manager_new_execution_path (diagnostic_manager *manager)
{
  FAIL_IF_NULL (manager);

  return manager->new_execution_path ();
}

/* Public entrypoint.  */

extern void
diagnostic_take_execution_path (diagnostic *diag,
				diagnostic_execution_path *path)
{
  FAIL_IF_NULL (diag);
  FAIL_IF_NULL (path);

  return diag->take_execution_path (path);
}

/* Public entrypoint.  */

void
diagnostic_execution_path_release (diagnostic_execution_path *path)
{
  delete path;
}

/* Public entrypoint.  */

diagnostic_event_id
diagnostic_execution_path_add_event (diagnostic_execution_path *path,
				     const diagnostic_physical_location *physical_loc,
				     const diagnostic_logical_location *logical_loc,
				     unsigned stack_depth,
				     const char *gmsgid, ...)
{
  FAIL_IF_NULL (path);
  FAIL_IF_NULL (gmsgid);

  va_list args;
  va_start (args, gmsgid);
  diagnostics::paths::event_id_t result
    = path->add_event_va (physical_loc,
			  logical_loc,
			  stack_depth,
			  nullptr,
			  gmsgid, &args);
  va_end (args);

  return as_diagnostic_event_id (result);
}

/* Public entrypoint.  */

diagnostic_event_id
diagnostic_execution_path_add_event_va (diagnostic_execution_path *path,
					const diagnostic_physical_location *physical_loc,
					const diagnostic_logical_location *logical_loc,
					unsigned stack_depth,
					const char *gmsgid,
					va_list *args)
{
  FAIL_IF_NULL (path);
  FAIL_IF_NULL (gmsgid);

  diagnostics::paths::event_id_t result
    = path->add_event_va (physical_loc,
			  logical_loc,
			  stack_depth,
			  nullptr,
			  gmsgid, args);
  return as_diagnostic_event_id (result);
}

/* Public entrypoint.  */

void
diagnostic_finish (diagnostic *diag, const char *gmsgid, ...)
{
  FAIL_IF_NULL (diag);

  va_list args;
  va_start (args, gmsgid);
  diagnostic_finish_va (diag, gmsgid, &args);
  va_end (args);
}

/* Public entrypoint.  */

void
diagnostic_finish_va (diagnostic *diag, const char *gmsgid, va_list *args)
{
  FAIL_IF_NULL (diag);

  if (const char *tool_name
      = diag->get_manager ().get_client_version_info ()->m_name.get_str ())
    progname = tool_name;
  else
    progname = "progname";
  auto_diagnostic_group d;
  diag->get_manager ().emit_va (*diag, gmsgid, args);
  delete diag;
}

/* Public entrypoint.  */

diagnostic_file *
diagnostic_physical_location_get_file (const diagnostic_physical_location *physical_loc)
{
  if (!physical_loc)
    return nullptr;

  return physical_loc->get_file ();
}

/* Public entrypoints for accessing logical location data.  */

enum diagnostic_logical_location_kind_t
diagnostic_logical_location_get_kind (const diagnostic_logical_location *loc)
{
  FAIL_IF_NULL (loc);

  return loc->m_kind;
}

const diagnostic_logical_location *
diagnostic_logical_location_get_parent (const diagnostic_logical_location *loc)
{
  FAIL_IF_NULL (loc);

  return loc->m_parent;
}

const char *
diagnostic_logical_location_get_short_name (const diagnostic_logical_location *loc)
{
  FAIL_IF_NULL (loc);

  return loc->m_short_name.get_str ();
}

const char *
diagnostic_logical_location_get_fully_qualified_name (const diagnostic_logical_location *loc)
{
  FAIL_IF_NULL (loc);

  return loc->m_fully_qualified_name.get_str ();
}

const char *
diagnostic_logical_location_get_decorated_name (const diagnostic_logical_location *loc)
{
  FAIL_IF_NULL (loc);

  return loc->m_decorated_name.get_str ();
}

namespace {

struct spec_context : public diagnostics::output_spec::context
{
public:
  spec_context (const char *option_name,
		const char *unparsed_spec,
		diagnostic_manager &affected_mgr,
		diagnostic_manager &control_mgr)
  : context (option_name, unparsed_spec, nullptr,
	     affected_mgr.get_line_table ()),
    m_control_mgr (control_mgr)
  {}

  void report_error_va (const char *gmsgid, va_list *ap) const final override
  {
    diagnostic *diag
      = diagnostic_begin (&m_control_mgr, DIAGNOSTIC_LEVEL_ERROR);
    diagnostic_finish_va (diag, gmsgid, ap);
  }

  const char *
  get_base_filename () const final override
  {
    return nullptr;
  }

private:
  diagnostic_manager &m_control_mgr;
};

} // anon namespace

/* Public entrypoint.  */

int
diagnostic_manager_add_sink_from_spec (diagnostic_manager *affected_mgr,
				       const char *option_name,
				       const char *spec,
				       diagnostic_manager *control_mgr)
{
  FAIL_IF_NULL (affected_mgr);
  FAIL_IF_NULL (option_name);
  FAIL_IF_NULL (spec);
  FAIL_IF_NULL (control_mgr);

  spec_context ctxt (option_name, spec, *affected_mgr, *control_mgr);
  auto inner_sink = ctxt.parse_and_make_sink (affected_mgr->get_dc ());
  if (!inner_sink)
    return -1;
  affected_mgr->get_dc ().add_sink (std::move (inner_sink));
  return 0;
}

/* Public entrypoint.  */

void
diagnostic_manager_set_analysis_target (diagnostic_manager *mgr,
					const diagnostic_file *file)
{
  FAIL_IF_NULL (mgr);
  FAIL_IF_NULL (file);

  mgr->get_dc ().set_main_input_filename (file->get_name ());
}

/* Public entrypoint.  */

diagnostic_node *
diagnostic_graph::add_node_with_id (std::string node_id,
				    diagnostic_node *parent_node)
{
  auto node_up
    = std::make_unique<diagnostic_node> (*this, std::move (node_id));
  diagnostic_node *new_node = node_up.get ();
  if (parent_node)
    parent_node->add_child (std::move (node_up));
  else
    add_node (std::move (node_up));
  return new_node;
}

/* Public entrypoint.  */

diagnostic_edge *
diagnostic_graph::add_edge_with_label (const char *edge_id,
				       diagnostic_node &src_node,
				       diagnostic_node &dst_node,
				       const char *label)
{
  auto edge_up
    = std::make_unique<diagnostic_edge> (*this, edge_id,
					 src_node, dst_node);
  diagnostic_edge *new_edge = edge_up.get ();
  if (label)
    new_edge->set_label (label);
  add_edge (std::move (edge_up));
  return new_edge;
}

/* Public entrypoint.  */

diagnostic_graph *
diagnostic_manager_new_graph (diagnostic_manager *manager)
{
  FAIL_IF_NULL (manager);

  return new diagnostic_graph (*manager);
}

/* Public entrypoint.  */

void
diagnostic_manager_take_global_graph (diagnostic_manager *manager,
				      diagnostic_graph *graph)
{
  FAIL_IF_NULL (manager);
  FAIL_IF_NULL (graph);

  manager->take_global_graph (std::unique_ptr<diagnostic_graph> (graph));
}

void
diagnostic_take_graph (diagnostic *diag,
		       diagnostic_graph *graph)
{
  FAIL_IF_NULL (diag);
  FAIL_IF_NULL (graph);

  diag->take_graph (std::unique_ptr<diagnostic_graph> (graph));
}

/* Public entrypoint.  */

void
diagnostic_graph_release (diagnostic_graph *graph)
{
  delete graph;
}

/* Public entrypoint.  */

void
diagnostic_graph_set_description (diagnostic_graph *graph,
				  const char *desc)
{
  FAIL_IF_NULL (graph);

  graph->set_description (desc);
}

/* Public entrypoint.  */

diagnostic_node *
diagnostic_graph_add_node (diagnostic_graph *graph,
			   const char *node_id,
			   diagnostic_node *parent_node)
{
  FAIL_IF_NULL (graph);
  FAIL_IF_NULL (node_id);

  return graph->add_node_with_id (node_id, parent_node);
}

/* Public entrypoint.  */

diagnostic_edge *
diagnostic_graph_add_edge (diagnostic_graph *graph,
			   const char *edge_id,
			   diagnostic_node *src_node,
			   diagnostic_node *dst_node,
			   const char *label)
{
  FAIL_IF_NULL (graph);
  FAIL_IF_NULL (src_node);
  FAIL_IF_NULL (dst_node);

  return graph->add_edge_with_label (edge_id, *src_node, *dst_node, label);
}

/* Public entrypoint.  */

diagnostic_node *
diagnostic_graph_get_node_by_id (diagnostic_graph *graph,
				 const char *node_id)
{
  FAIL_IF_NULL (graph);
  FAIL_IF_NULL (node_id);

  return static_cast<diagnostic_node *> (graph->get_node_by_id (node_id));
}

/* Public entrypoint.  */

diagnostic_edge *
diagnostic_graph_get_edge_by_id (diagnostic_graph *graph,
				 const char *edge_id)
{
  FAIL_IF_NULL (graph);
  FAIL_IF_NULL (edge_id);

  return static_cast<diagnostic_edge *> (graph->get_edge_by_id (edge_id));
}

/* Public entrypoint.  */

void
diagnostic_node_set_location (diagnostic_node *node,
			      const diagnostic_physical_location *loc)
{
  FAIL_IF_NULL (node);

  node->set_physical_loc (loc ? loc->m_inner : UNKNOWN_LOCATION);
}

/* Public entrypoint.  */

void
diagnostic_node_set_label (diagnostic_node *node,
			   const char *label)
{
  FAIL_IF_NULL (node);

  node->set_label (label);
}

/* Public entrypoint.  */

void
diagnostic_node_set_logical_location (diagnostic_node *node,
				      const diagnostic_logical_location *logical_loc)
{
  FAIL_IF_NULL (node);

  node->set_logical_loc
    (impl_logical_location_manager::key_from_ptr (logical_loc));
}

/* Private entrypoint.  */

void
private_diagnostic_graph_set_property_bag (diagnostic_graph &graph,
					  std::unique_ptr<json::object> properties)
{
  graph.set_property_bag (std::move (properties));
}

/* Private entrypoint.  */

void
private_diagnostic_node_set_property_bag (diagnostic_node &node,
					  std::unique_ptr<json::object> properties)
{
  node.set_property_bag (std::move (properties));
}

/* Private entrypoint.  */

void
private_diagnostic_edge_set_property_bag (diagnostic_edge &edge,
					  std::unique_ptr<json::object> properties)
{
  edge.set_property_bag (std::move (properties));
}

/* Public entrypoint.  */

diagnostic_message_buffer *
diagnostic_message_buffer_new ()
{
  return new diagnostic_message_buffer ();
}

/* Public entrypoint.  */

void
diagnostic_message_buffer_release (diagnostic_message_buffer *msg_buf)
{
  FAIL_IF_NULL (msg_buf);
  delete msg_buf;
}

void
diagnostic_message_buffer_append_str (diagnostic_message_buffer *msg_buf,
				      const char *p)
{
  FAIL_IF_NULL (msg_buf);
  FAIL_IF_NULL (p);
  msg_buf->m_tokens.push_back_text (label_text::take (xstrdup (p)));
}

/* Public entrypoint.  */

void
diagnostic_message_buffer_append_text (diagnostic_message_buffer *msg_buf,
				       const char *p,
				       size_t len)
{
  FAIL_IF_NULL (msg_buf);
  FAIL_IF_NULL (p);
  msg_buf->m_tokens.push_back_text (label_text::take (xstrndup (p, len)));
}

/* Public entrypoint.  */

void
diagnostic_message_buffer_append_byte (diagnostic_message_buffer *msg_buf,
				       char ch)
{
  FAIL_IF_NULL (msg_buf);
  msg_buf->m_tokens.push_back_byte (ch);
}

/* Public entrypoint.  */

void
diagnostic_message_buffer_append_printf (diagnostic_message_buffer *msg_buf,
					 const char *fmt, ...)
{
  FAIL_IF_NULL (msg_buf);
  FAIL_IF_NULL (fmt);

  va_list args;
  va_start (args, fmt);

  char *formatted_buf = xvasprintf (fmt, args);

  va_end (args);

  msg_buf->m_tokens.push_back_text (label_text::take (formatted_buf));
}

/* Public entrypoint.  */

void
diagnostic_message_buffer_append_event_id (diagnostic_message_buffer *msg_buf,
					   diagnostic_event_id event_id)
{
  FAIL_IF_NULL (msg_buf);
  msg_buf->m_tokens.push_back<pp_token_event_id> (event_id);
}

/* Public entrypoint.  */

void
diagnostic_message_buffer_begin_url (diagnostic_message_buffer *msg_buf,
				     const char *url)
{
  FAIL_IF_NULL (msg_buf);
  FAIL_IF_NULL (url);
  msg_buf->m_tokens.push_back<pp_token_begin_url>
    (label_text::take (xstrdup (url)));
}

/* Public entrypoint.  */

void
diagnostic_message_buffer_end_url (diagnostic_message_buffer *msg_buf)
{
  FAIL_IF_NULL (msg_buf);
  msg_buf->m_tokens.push_back<pp_token_end_url> ();
}

/* Public entrypoint.  */

void
diagnostic_message_buffer_begin_quote (diagnostic_message_buffer *msg_buf)
{
  FAIL_IF_NULL (msg_buf);
  msg_buf->m_tokens.push_back<pp_token_begin_quote> ();
}

/* Public entrypoint.  */

void
diagnostic_message_buffer_end_quote (diagnostic_message_buffer *msg_buf)
{
  FAIL_IF_NULL (msg_buf);
  msg_buf->m_tokens.push_back<pp_token_end_quote> ();
}

/* Public entrypoint.  */

void
diagnostic_message_buffer_begin_color (diagnostic_message_buffer *msg_buf,
				       const char *color)
{
  FAIL_IF_NULL (msg_buf);
  FAIL_IF_NULL (color);
  msg_buf->m_tokens.push_back<pp_token_begin_color>
    (label_text::take (xstrdup (color)));
}

/* Public entrypoint.  */

void
diagnostic_message_buffer_end_color (diagnostic_message_buffer *msg_buf)
{
  FAIL_IF_NULL (msg_buf);
  msg_buf->m_tokens.push_back<pp_token_end_color> ();
}

/* Public entrypoint.  */

void
diagnostic_message_buffer_dump (const diagnostic_message_buffer *msg_buf,
				FILE *outf)
{
  FAIL_IF_NULL (msg_buf);
  FAIL_IF_NULL (outf);

  msg_buf->m_tokens.dump (outf);
}

/* Public entrypoint.  */

void
diagnostic_finish_via_msg_buf (diagnostic *diag,
			       diagnostic_message_buffer *msg_buf)
{
  FAIL_IF_NULL (diag);
  FAIL_IF_NULL (msg_buf);

  if (const char *tool_name
      = diag->get_manager ().get_client_version_info ()->m_name.get_str ())
    progname = tool_name;
  else
    progname = "progname";
  auto_diagnostic_group d;
  diag->get_manager ().emit_msg_buf (*diag, *msg_buf);
  delete diag;
  delete msg_buf;
}

/* Public entrypoint.  */

void
diagnostic_add_location_with_label_via_msg_buf (diagnostic *diag,
						const diagnostic_physical_location *loc,
						diagnostic_message_buffer *msg_buf)
{
  FAIL_IF_NULL (diag);
  diag->get_manager ().assert_valid_diagnostic_physical_location (loc);
  FAIL_IF_NULL (msg_buf);

  std::unique_ptr<diagnostic_message_buffer> msg_buf_up (msg_buf);
  diag->add_location_with_label (loc, std::move (msg_buf_up));
}

/* Public entrypoint.  */

diagnostic_event_id
diagnostic_execution_path_add_event_via_msg_buf (diagnostic_execution_path *path,
						 const diagnostic_physical_location *physical_loc,
						 const diagnostic_logical_location *logical_loc,
						 unsigned stack_depth,
						 diagnostic_message_buffer *msg_buf)
{
  FAIL_IF_NULL (path);
  FAIL_IF_NULL (msg_buf);

  std::unique_ptr<diagnostic_message_buffer> msg_buf_up (msg_buf);
  diagnostic_event_id_t result
    = path->add_event_via_msg_buf (physical_loc,
				   logical_loc,
				   stack_depth,
				   nullptr,
				   std::move (msg_buf_up));
  return as_diagnostic_event_id (result);
}

/* Public entrypoint.  */

void
diagnostic_graph_set_description_via_msg_buf (diagnostic_graph *graph,
					      diagnostic_message_buffer *desc)
{
  FAIL_IF_NULL (graph);

  if (desc)
    graph->set_description (desc->to_string ());
  else
    graph->set_description (nullptr);
}

/* Public entrypoint.  */

diagnostic_edge *
diagnostic_graph_add_edge_via_msg_buf (diagnostic_graph *graph,
				       const char *edge_id,
				       diagnostic_node *src_node,
				       diagnostic_node *dst_node,
				       diagnostic_message_buffer *label)
{
  FAIL_IF_NULL (graph);
  FAIL_IF_NULL (src_node);
  FAIL_IF_NULL (dst_node);

  if (label)
    {
      std::string label_str (label->to_string ());
      return graph->add_edge_with_label (edge_id, *src_node, *dst_node,
					 label_str.c_str ());
    }
  else
    return graph->add_edge_with_label (edge_id, *src_node, *dst_node,
				       nullptr);
}

/* Public entrypoint.  */

void
diagnostic_node_set_label_via_msg_buf (diagnostic_node *node,
				       diagnostic_message_buffer *label)
{
  FAIL_IF_NULL (node);

  if (label)
    node->set_label (label->to_string ());
  else
    node->set_label (nullptr);
}

/* Private entrypoint.  */

diagnostic_event_id
private_diagnostic_execution_path_add_event_3 (diagnostic_execution_path *path,
					       const diagnostic_physical_location *physical_loc,
					       const diagnostic_logical_location *logical_loc,
					       unsigned stack_depth,
					       diagnostic_graph *state_graph,
					       diagnostic_message_buffer *msg_buf)
{
  FAIL_IF_NULL (path);
  FAIL_IF_NULL (msg_buf);

  diagnostic_event_id_t result
    = path->add_event_via_msg_buf
	(physical_loc,
	 logical_loc,
	 stack_depth,
	 std::unique_ptr <diagnostic_graph> (state_graph),
	 std::unique_ptr <diagnostic_message_buffer> (msg_buf));

  return as_diagnostic_event_id (result);
}

/* Public entrypoint.  */

void
diagnostic_manager_set_debug_physical_locations (diagnostic_manager *mgr,
						 int value)
{
  FAIL_IF_NULL (mgr);
  mgr->set_debug_physical_locations (value);
}

/* Private entrypoint.  */

void
private_diagnostic_set_nesting_level (diagnostic *diag,
				      int nesting_level)
{
  FAIL_IF_NULL (diag);
  diag->set_nesting_level (nesting_level);
}
