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
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "intl.h"
#include "diagnostic.h"
#include "diagnostic-color.h"
#include "diagnostic-url.h"
#include "diagnostic-metadata.h"
#include "diagnostic-path.h"
#include "diagnostic-client-data-hooks.h"
#include "diagnostic-format-sarif.h"
#include "diagnostic-format-text.h"
#include "logical-location.h"
#include "edit-context.h"
#include "make-unique.h"
#include "libgdiagnostics.h"

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

struct diagnostic_logical_location : public logical_location
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

  const char *get_short_name () const final override
  {
    return m_short_name.get_str ();
  }
  const char *get_name_with_scope () const final override
  {
    return m_fully_qualified_name.get_str ();
  }
  const char *get_internal_name () const final override
  {
    return m_decorated_name.get_str ();
  }
  enum logical_location_kind get_kind () const final override
  {
    switch (m_kind)
      {
      default:
	gcc_unreachable ();
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION:
	return LOGICAL_LOCATION_KIND_FUNCTION;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_MEMBER:
	return LOGICAL_LOCATION_KIND_MEMBER;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_MODULE:
	return LOGICAL_LOCATION_KIND_MODULE;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_NAMESPACE:
	return LOGICAL_LOCATION_KIND_NAMESPACE;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_TYPE:
	return LOGICAL_LOCATION_KIND_TYPE;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_RETURN_TYPE:
	return LOGICAL_LOCATION_KIND_RETURN_TYPE;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_PARAMETER:
	return LOGICAL_LOCATION_KIND_PARAMETER;
      case DIAGNOSTIC_LOGICAL_LOCATION_KIND_VARIABLE:
	return LOGICAL_LOCATION_KIND_VARIABLE;
      }
  }

  enum diagnostic_logical_location_kind_t get_external_kind () const
  {
    return m_kind;
  }

  const diagnostic_logical_location *get_parent () const { return m_parent; }

  label_text get_name_for_path_output () const
  {
    return label_text::borrow (m_short_name.get_str ());
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

private:
  enum diagnostic_logical_location_kind_t m_kind;
  const diagnostic_logical_location *m_parent;
  owned_nullable_string m_short_name;
  owned_nullable_string m_fully_qualified_name;
  owned_nullable_string m_decorated_name;
};

static diagnostic_event_id
as_diagnostic_event_id (diagnostic_event_id_t id)
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

  diagnostic_source_printing_options &get_source_printing_options ()
  {
    return m_source_printing;
  }

  void
  set_colorize (enum diagnostic_colorize colorize);

  static void
  text_starter (diagnostic_text_output_format &text_output,
		const diagnostic_info *diagnostic);

private:
  diagnostic_text_output_format *m_inner_sink; // borrowed from dc
  diagnostic_source_printing_options m_source_printing;
};

class sarif_sink : public sink
{
public:
  sarif_sink (diagnostic_manager &mgr,
	      FILE *dst_stream,
	      const diagnostic_file *main_input_file,
	      enum sarif_version version);
};

/* Helper for the linemap code.  */

static size_t
round_alloc_size (size_t s)
{
  return s;
}

class impl_diagnostic_client_data_hooks : public diagnostic_client_data_hooks
{
public:
  impl_diagnostic_client_data_hooks (diagnostic_manager &mgr)
  : m_mgr (mgr)
  {}

  const client_version_info *get_any_version_info () const final override;
  const logical_location *get_current_logical_location () const final override;
  const char * maybe_get_sarif_source_language (const char *filename)
    const final override;
  void add_sarif_invocation_properties (sarif_object &invocation_obj)
    const final override;

private:
  diagnostic_manager &m_mgr;
};

class impl_client_version_info : public client_version_info
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
    m_prev_diag_logical_loc (nullptr)
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
    m_dc.m_source_printing.enabled = true;
    m_dc.m_source_printing.colorize_source_p = true;

    /* We don't currently expose a way for clients to manipulate the
       following.  */
    m_dc.m_source_printing.show_labels_p = true;
    m_dc.m_source_printing.show_line_numbers_p = true;
    m_dc.m_source_printing.min_margin_width = 6;
    m_dc.set_path_format (DPF_INLINE_EVENTS);

    m_dc.m_client_aux_data = this;
    m_dc.set_client_data_hooks
      (::make_unique<impl_diagnostic_client_data_hooks> (*this));

    diagnostic_text_starter (&m_dc) = diagnostic_text_sink::text_starter;

    m_edit_context = ::make_unique <edit_context> (m_dc.get_file_cache ());
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
  diagnostic_context &get_dc () { return m_dc; }

  void write_patch (FILE *dst_stream);

  void add_sink (std::unique_ptr<sink> sink)
  {
    m_sinks.push_back (std::move (sink));
  }

  void emit (diagnostic &diag, const char *msgid, va_list *args)
    LIBGDIAGNOSTICS_PARAM_GCC_FORMAT_STRING(3, 0);

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

  const diagnostic_physical_location *
  new_location_from_file_and_line (const diagnostic_file *file,
				   diagnostic_line_num_t line_num)
  {
    ensure_linemap_for_file_and_line (file, line_num);
    location_t loc = linemap_position_for_column (&m_line_table, 0);
    return new_location (loc);
  }

  const diagnostic_physical_location *
  new_location_from_file_line_column (const diagnostic_file *file,
				      diagnostic_line_num_t line_num,
				      diagnostic_column_num_t column_num)
  {
    ensure_linemap_for_file_and_line (file, line_num);
    location_t loc = linemap_position_for_column (&m_line_table, column_num);
    return new_location (loc);
  }

  const diagnostic_physical_location *
  new_location_from_range (const diagnostic_physical_location *loc_caret,
			   const diagnostic_physical_location *loc_start,
			   const diagnostic_physical_location *loc_end)
  {
    return new_location
      (m_line_table.make_location (as_location_t (loc_caret),
				   as_location_t (loc_start),
				   as_location_t (loc_end)));
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
      = ::make_unique<diagnostic_logical_location> (kind,
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

  const client_version_info *get_client_version_info () const
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
	line_map *map
	  = const_cast<line_map *>
	    (linemap_add (&m_line_table, LC_RENAME_VERBATIM, false,
			  file->get_name (), 0));
	((line_map_ordinary *)map)->included_from = UNKNOWN_LOCATION;
      }
    linemap_line_start (&m_line_table, linenum, 100);
  }

  const diagnostic_physical_location *
  new_location (location_t loc)
  {
    if (loc == UNKNOWN_LOCATION)
      return nullptr;
    if (diagnostic_physical_location **slot = m_location_t_map.get (loc))
      return *slot;
    diagnostic_physical_location *phys_loc
      = new diagnostic_physical_location (this, loc);
    m_location_t_map.put (loc, phys_loc);
    return phys_loc;
  }

  diagnostic_context m_dc;
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
  std::unique_ptr<edit_context> m_edit_context;
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

class impl_rule : public diagnostic_metadata::rule
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

class libgdiagnostics_path_event : public diagnostic_event
{
public:
  libgdiagnostics_path_event (const diagnostic_physical_location *physical_loc,
			      const diagnostic_logical_location *logical_loc,
			      unsigned stack_depth,
			      const char *gmsgid,
			      va_list *args)
  : m_physical_loc (physical_loc),
    m_logical_loc (logical_loc),
    m_stack_depth (stack_depth)
  {
    m_desc_uncolored = make_desc (gmsgid, args, false);
    m_desc_colored = make_desc (gmsgid, args, true);
  }

  /* diagnostic_event vfunc implementations.  */

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
    if (pp_show_color (&pp))
      pp_string (&pp, m_desc_colored.get ());
    else
      pp_string (&pp, m_desc_uncolored.get ());
  }

  const logical_location *get_logical_location () const
  {
    return m_logical_loc;
  }

  meaning get_meaning () const final override
  {
    return meaning ();
  }

  bool connect_to_next_event_p () const final override
  {
    return false; // TODO
  }

  diagnostic_thread_id_t get_thread_id () const final override
  {
    return 0;
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
  label_text m_desc_uncolored;
  label_text m_desc_colored;
};

class libgdiagnostics_path_thread : public diagnostic_thread
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

struct diagnostic_execution_path : public diagnostic_path
{
  diagnostic_execution_path ()
  : m_thread ("")
  {
  }

  diagnostic_event_id_t
  add_event_va (const diagnostic_physical_location *physical_loc,
		const diagnostic_logical_location *logical_loc,
		unsigned stack_depth,
		const char *gmsgid,
		va_list *args)
  {
    m_events.push_back (::make_unique<libgdiagnostics_path_event> (physical_loc,
								   logical_loc,
								   stack_depth,
								   gmsgid,
								   args));
    return m_events.size () - 1;
  }

  /* diagnostic_path vfunc implementations.  */

  unsigned num_events () const final override
  {
    return m_events.size ();
  }
  const diagnostic_event & get_event (int idx) const final override
  {
    return *m_events[idx];
  }
  unsigned num_threads () const final override { return 1; }
  const diagnostic_thread &
  get_thread (diagnostic_thread_id_t) const final override
  {
    return m_thread;
  }

  bool
  same_function_p (int event_idx_a,
		   int event_idx_b) const final override
  {
    const logical_location *logical_loc_a
      = m_events[event_idx_a]->get_logical_location ();
    const logical_location *logical_loc_b
      = m_events[event_idx_b]->get_logical_location ();

    /* Pointer equality, as we uniqify logical location instances.  */
    return logical_loc_a == logical_loc_b;
  }

private:
  libgdiagnostics_path_thread m_thread;
  std::vector<std::unique_ptr<libgdiagnostics_path_event>> m_events;
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
    m_path (nullptr)
  {}

  diagnostic_manager &get_manager () const
  {
    return m_diag_mgr;
  }

  enum diagnostic_level get_level () const { return m_level; }

  rich_location *get_rich_location () { return &m_rich_loc; }
  const diagnostic_metadata *get_metadata () { return &m_metadata; }

  void set_cwe (unsigned cwe_id)
  {
    m_metadata.add_cwe (cwe_id);
  }

  void add_rule (const char *title,
		 const char *url)
  {
    std::unique_ptr<impl_rule> rule = ::make_unique<impl_rule> (title, url);
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
      = ::make_unique <impl_range_label> (text);
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
    m_path = ::make_unique<diagnostic_execution_path> ();
    m_rich_loc.set_path (m_path.get ());
    return m_path.get ();
  }

  void
  take_execution_path (diagnostic_execution_path *path)
  {
    m_path = std::unique_ptr<diagnostic_execution_path> (path);
    m_rich_loc.set_path (path);
  }

private:
  diagnostic_manager &m_diag_mgr;
  enum diagnostic_level m_level;
  impl_rich_location m_rich_loc;
  const diagnostic_logical_location *m_logical_loc;
  diagnostic_metadata m_metadata;
  std::vector<std::unique_ptr<range_label>> m_labels;
  std::vector<std::unique_ptr<impl_rule>> m_rules;
  std::unique_ptr<diagnostic_execution_path> m_path;
};

static diagnostic_t
diagnostic_t_from_diagnostic_level (enum diagnostic_level level)
{
  switch (level)
    {
    default:
      gcc_unreachable ();
    case DIAGNOSTIC_LEVEL_ERROR:
      return DK_ERROR;
    case DIAGNOSTIC_LEVEL_WARNING:
      return DK_WARNING;
    case DIAGNOSTIC_LEVEL_NOTE:
      return DK_NOTE;
    case DIAGNOSTIC_LEVEL_SORRY:
      return DK_SORRY;
    }
}

void
diagnostic_file::set_buffered_content (const char *buf, size_t sz)
{
  m_content = ::make_unique<content_buffer> (buf, sz);

  // Populate file_cache:
  file_cache &fc = m_mgr.get_dc ().get_file_cache ();
  fc.add_buffered_content (m_name.get_str (), buf, sz);
}

/* class impl_diagnostic_client_data_hooks.  */

const client_version_info *
impl_diagnostic_client_data_hooks::get_any_version_info () const
{
  return m_mgr.get_client_version_info ();
}

const logical_location *
impl_diagnostic_client_data_hooks::get_current_logical_location () const
{
  gcc_assert (m_mgr.get_current_diag ());

  return m_mgr.get_current_diag ()->get_logical_location ();
}

const char *
impl_diagnostic_client_data_hooks::
maybe_get_sarif_source_language (const char *filename) const
{
  return m_mgr.maybe_get_sarif_source_language (filename);
}

void
impl_diagnostic_client_data_hooks::
add_sarif_invocation_properties (sarif_object &) const
{
  // No-op.
}

/* struct diagnostic_text_sink : public sink.  */

diagnostic_text_sink::diagnostic_text_sink (diagnostic_manager &mgr,
					    FILE *dst_stream,
					    enum diagnostic_colorize colorize)
: sink (mgr),
  m_source_printing (mgr.get_dc ().m_source_printing)
{
  auto inner_sink
    = ::make_unique<diagnostic_text_output_format> (mgr.get_dc (),
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
diagnostic_text_sink::text_starter (diagnostic_text_output_format &text_output,
				    const diagnostic_info *info)
{
  gcc_assert (info->x_data);
  const diagnostic &diag = *static_cast<const diagnostic *> (info->x_data);
  pretty_printer *pp = text_output.get_printer ();
  const diagnostic_logical_location *diag_logical_loc
    = diag.get_logical_location ();
  diagnostic_manager &mgr = diag.get_manager ();
  if (diag_logical_loc && diag_logical_loc != mgr.get_prev_diag_logical_loc ())
    {
      pp_set_prefix (pp, nullptr);
      switch (diag_logical_loc->get_kind ())
	{
	default:
	  break;
	case LOGICAL_LOCATION_KIND_FUNCTION:
	  if (const char *name
	      = diag_logical_loc->get_name_with_scope ())
	    {
	      pp_printf (pp, _("In function %qs"), name);
	      pp_character (pp, ':');
	      pp_newline (pp);
	    }
	  break;
	  // TODO: handle other cases
	}
    }
  pp_set_prefix (pp,
		 text_output.build_prefix (*info));
}

/* class sarif_sink : public sink.  */

sarif_sink::sarif_sink (diagnostic_manager &mgr,
			FILE *dst_stream,
			const diagnostic_file *main_input_file,
			enum sarif_version version)
: sink (mgr)
{
  diagnostic_output_file output_file (dst_stream, false,
				      label_text::borrow ("sarif_sink"));
  auto inner_sink = make_sarif_sink (mgr.get_dc (),
				     *mgr.get_line_table (),
				     main_input_file->get_name (),
				     version,
				     std::move (output_file));
  mgr.get_dc ().add_sink (std::move (inner_sink));
}

/* struct diagnostic_manager.  */

void
diagnostic_manager::write_patch (FILE *dst_stream)
{
  pretty_printer pp;
  pp.set_output_stream (dst_stream);
  m_edit_context->print_diff (&pp, true);
  pp_flush (&pp);
}

void
diagnostic_manager::emit (diagnostic &diag, const char *msgid, va_list *args)
{
  set_line_table_global ();

  m_current_diag = &diag;

  {
    m_dc.begin_group ();

    diagnostic_info info;
GCC_DIAGNOSTIC_PUSH_IGNORED(-Wsuggest-attribute=format)
    diagnostic_set_info (&info, msgid, args, diag.get_rich_location (),
			 diagnostic_t_from_diagnostic_level (diag.get_level ()));
GCC_DIAGNOSTIC_POP
    info.metadata = diag.get_metadata ();
    info.x_data = &diag;
    diagnostic_report_diagnostic (&m_dc, &info);

    m_dc.end_group ();
  }

  rich_location *rich_loc = diag.get_rich_location ();
  if (rich_loc->fixits_can_be_auto_applied_p ())
    m_edit_context->add_fixits (rich_loc);

  m_prev_diag_logical_loc = diag.get_logical_location ();
  m_current_diag = nullptr;
}

diagnostic_execution_path *
diagnostic_manager::new_execution_path ()
{
  return new diagnostic_execution_path ();
}

/* Error-checking at the API boundary.  */

#define FAIL_IF_NULL(PTR_ARG) \
  do {						    \
    volatile const void *p = (PTR_ARG);		    \
    if (!p) {					    \
      fprintf (stderr, "%s: %s must be non-NULL\n",   \
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

  enum sarif_version internal_version;
  switch (version)
    {
    default:
      fprintf (stderr, "%s: unrecognized value for version: %i\n",
	       __func__, (int)version);
      abort ();
    case DIAGNOSTIC_SARIF_VERSION_2_1_0:
      internal_version = sarif_version::v2_1_0;
      break;
    case DIAGNOSTIC_SARIF_VERSION_2_2_PRERELEASE:
      internal_version = sarif_version::v2_2_prerelease_2024_08_08;
      break;
    }

  diag_mgr->add_sink (make_unique<sarif_sink> (*diag_mgr,
					       dst_stream,
					       main_input_file,
					       internal_version));
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
	fprintf (out, ", content=(size=%zi)", buf->m_sz);
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

      diagnostic_context dc;
      diagnostic_initialize (&dc, 0);
      dc.m_show_column = true;

      diagnostic_text_output_format text_format (dc);
      label_text loc_text = text_format.get_location_text (exp_loc);
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
      switch (loc->get_external_kind ())
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
	}
      if (const diagnostic_logical_location *parent = loc->get_parent ())
	diagnostic_manager_debug_dump_logical_location (diag_mgr,
							parent,
							out);
      if (const char *val = loc->get_short_name ())
	fprintf (out, ", short_name=\"%s\"", val);
      if (const char *val = loc->get_name_with_scope ())
	fprintf (out, ", fully_qualified_name=\"%s\"", val);
      if (const char *val = loc->get_internal_name ())
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
  diagnostic_event_id_t result = path->add_event_va (physical_loc,
						     logical_loc,
						     stack_depth,
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

  diagnostic_event_id_t result = path->add_event_va (physical_loc,
						     logical_loc,
						     stack_depth,
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
  diag->get_manager ().emit (*diag, gmsgid, args);
  delete diag;
}
