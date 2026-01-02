/* A C++ wrapper API around libgdiagnostics.h for emitting diagnostics.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef LIBGDIAGNOSTICSPP_H
#define LIBGDIAGNOSTICSPP_H

#include "libgdiagnostics.h"

namespace libgdiagnostics {

typedef diagnostic_line_num_t line_num_t;
typedef diagnostic_column_num_t column_num_t;

class file;
class physical_location;
class logical_location;
class execution_path;
class group;
class manager;
class diagnostic;
class graph;
class node;
class edge;
class message_buffer;

/* Wrapper around a borrowed diagnostic_text_sink *.  */

class text_sink
{
public:
  text_sink (diagnostic_text_sink *inner)
  : m_inner (inner)
  {
  }

  void
  set_source_printing_enabled (int value)
  {
    diagnostic_text_sink_set_source_printing_enabled (m_inner, value);
  }

  void
  set_colorize (enum diagnostic_colorize colorize)
  {
    diagnostic_text_sink_set_colorize (m_inner, colorize);
  }

  void
  set_labelled_source_colorization_enabled (int value)
  {
    diagnostic_text_sink_set_labelled_source_colorization_enabled (m_inner,
								   value);
  }

  diagnostic_text_sink *m_inner;
};

/* Wrapper around a diagnostic_file *.  */

class file
{
public:
  file () : m_inner (nullptr) {}
  file (diagnostic_file *file) : m_inner (file) {}
  file (const file &other) : m_inner (other.m_inner) {}
  file &operator= (const file &other) { m_inner = other.m_inner; return *this; }

  void set_buffered_content (const char *data, size_t sz);

  diagnostic_file * m_inner;
};

/* Wrapper around a const diagnostic_physical_location *.  */

class physical_location
{
public:
  physical_location () : m_inner (nullptr) {}

  physical_location (const diagnostic_physical_location *location)
  : m_inner (location)
  {}

  file get_file () const;

  const diagnostic_physical_location *m_inner;
};

/* Wrapper around a const diagnostic_logical_location *.  */

class logical_location
{
public:
  logical_location () : m_inner (nullptr) {}

  logical_location (const diagnostic_logical_location *logical_loc)
  : m_inner (logical_loc)
  {}

  operator bool() { return m_inner != nullptr; }

  // Various accessors
  enum diagnostic_logical_location_kind_t get_kind () const;
  logical_location get_parent () const;
  const char *get_short_name () const;
  const char *get_fully_qualified_name () const;
  const char *get_decorated_name () const;

  bool operator== (const logical_location &other) const
  {
    return m_inner == other.m_inner;
  }

  bool operator!= (const logical_location &other) const
  {
    return m_inner != other.m_inner;
  }

  const diagnostic_logical_location *m_inner;
};

/* Wrapper around a diagnostic_message_buffer *, with ownership.  */

class message_buffer
{
public:
  message_buffer () : m_inner (nullptr) {}
  message_buffer (diagnostic_message_buffer *inner) : m_inner (inner) {}
  ~message_buffer ()
  {
    if (m_inner)
      diagnostic_message_buffer_release (m_inner);
  }
  message_buffer (const message_buffer &) = delete;
  message_buffer (message_buffer &&other)
  {
    m_inner = other.m_inner;
    other.m_inner = nullptr;
  }
  message_buffer& operator= (const message_buffer &) = delete;
  message_buffer& operator= (message_buffer &&other)
  {
    if (m_inner)
      diagnostic_message_buffer_release (m_inner);
    m_inner = other.m_inner;
    other.m_inner = nullptr;
    return *this;
  }

  message_buffer&
  operator+= (const char *str)
  {
    diagnostic_message_buffer_append_str (m_inner, str);
    return *this;
  }

  message_buffer&
  operator+= (char ch)
  {
    diagnostic_message_buffer_append_byte (m_inner, ch);
    return *this;
  }

  message_buffer &
  begin_url (const char *url)
  {
    diagnostic_message_buffer_begin_url (m_inner, url);
    return *this;
  }

  message_buffer &
  end_url ()
  {
    diagnostic_message_buffer_end_url (m_inner);
    return *this;
  }

  diagnostic_message_buffer *m_inner;
};

/* RAII class around a diagnostic_execution_path *.  */

class execution_path
{
public:
  execution_path () : m_inner (nullptr), m_owned (false) {}

  execution_path (diagnostic_execution_path *path)
  : m_inner (path), m_owned (true)
  {}

  execution_path (const diagnostic_execution_path *path)
  : m_inner (const_cast<diagnostic_execution_path *> (path)),
    m_owned (false)
  {}

  execution_path (const execution_path &other) = delete;
  execution_path &operator= (const execution_path &other) = delete;

  execution_path (execution_path &&other)
  : m_inner (other.m_inner),
    m_owned (other.m_owned)
  {
    other.m_inner = nullptr;
    other.m_owned = false;
  }

  execution_path &operator= (execution_path &&other)
  {
    m_inner = other.m_inner;
    m_owned = other.m_owned;
    other.m_inner = nullptr;
    other.m_owned = false;
    return *this;
  }

  ~execution_path ()
  {
    if (m_owned)
      diagnostic_execution_path_release (m_inner);
  }

  diagnostic_event_id
  add_event (physical_location physical_loc,
	     logical_location logical_loc,
	     unsigned stack_depth,
	     const char *fmt, ...)
    LIBGDIAGNOSTICS_PARAM_GCC_FORMAT_STRING (5, 6);

  diagnostic_event_id
  add_event_va (physical_location physical_loc,
		logical_location logical_loc,
		unsigned stack_depth,
		const char *fmt,
		va_list *args)
    LIBGDIAGNOSTICS_PARAM_GCC_FORMAT_STRING (5, 0);

  diagnostic_event_id
  add_event_via_msg_buf (physical_location physical_loc,
			 logical_location logical_loc,
			 unsigned stack_depth,
			 message_buffer &&msg_buf);

  diagnostic_execution_path *m_inner;
  bool m_owned;
};

/* RAII class for starting/ending a group within a diagnostic_manager.  */

class group
{
public:
  group (manager &mgr);
  ~group ();

private:
  manager &m_mgr;
};

/* Wrapper around a diagnostic *.  */

class diagnostic
{
public:
  diagnostic (::diagnostic *d) : m_inner (d) {}

  void
  set_cwe (unsigned cwe_id);

  void
  add_rule (const char *title, const char *url);

  void
  set_location (physical_location loc);

  void
  add_location (physical_location loc);

  void
  add_location_with_label (physical_location loc,
			   const char *text);

  void
  add_location_with_label (physical_location loc,
			   message_buffer &&text);

  void
  set_logical_location (logical_location loc);

  void
  add_fix_it_hint_insert_before (physical_location loc,
				 const char *addition);
  void
  add_fix_it_hint_insert_after (physical_location loc,
				const char *addition);
  void
  add_fix_it_hint_replace (physical_location loc,
			   const char *replacement);
  void
  add_fix_it_hint_delete (physical_location loc);

  void
  take_execution_path (execution_path path);

  void
  take_graph (graph g);

  void
  finish (const char *fmt, ...)
    LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2)
    LIBGDIAGNOSTICS_PARAM_GCC_FORMAT_STRING (2, 3);

  void
  finish_va (const char *fmt, va_list *args)
    LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2)
    LIBGDIAGNOSTICS_PARAM_GCC_FORMAT_STRING (2, 0);

  void
  finish_via_msg_buf (message_buffer &&msg_buf);

  ::diagnostic * const m_inner;
};

/* Wrapper around a diagnostic_manager *, possibly with ownership.  */

class manager
{
public:
  manager ()
  : m_inner (diagnostic_manager_new ()),
    m_owned (true)
  {
  }
  manager (diagnostic_manager *inner, bool owned)
  : m_inner (inner),
    m_owned (owned)
  {
  }
  ~manager ()
  {
    if (m_owned)
      diagnostic_manager_release (m_inner);
  }

  manager (const manager &other) = delete;
  manager (manager &&other)
  : m_inner (other.m_inner),
    m_owned (other.m_owned)
  {
    other.m_inner = nullptr;
  }

  void
  set_tool_name (const char *value)
  {
    diagnostic_manager_set_tool_name (m_inner, value);
  }

  void
  set_full_name (const char *value)
  {
    diagnostic_manager_set_full_name (m_inner, value);
  }

  void
  set_version_string (const char *value)
  {
    diagnostic_manager_set_version_string (m_inner, value);
  }

  void
  set_version_url (const char *value)
  {
    diagnostic_manager_set_version_url (m_inner, value);
  }

  text_sink
  add_text_sink (FILE *dst_stream,
		 enum diagnostic_colorize colorize)
  {
    return text_sink
      (diagnostic_manager_add_text_sink (m_inner, dst_stream, colorize));
  }

  void
  add_sarif_sink (FILE *dst_stream,
		  file main_input_file,
		  enum diagnostic_sarif_version version)
  {
    diagnostic_manager_add_sarif_sink (m_inner, dst_stream,
				       main_input_file.m_inner,
				       version);
  }

  bool
  add_sink_from_spec (const char *option_name,
		      const char *spec,
		      manager control_mgr)
  {
    return diagnostic_manager_add_sink_from_spec (m_inner,
						  option_name,
						  spec,
						  control_mgr.m_inner);
  }

  void
  write_patch (FILE *dst_stream)
  {
    diagnostic_manager_write_patch (m_inner, dst_stream);
  }

  /* Location management.  */

  file
  new_file (const char *name,
	    const char *sarif_source_language)
    LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2)
    LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (3);

  void
  debug_dump (file f,
	      FILE *out);

  physical_location
  new_location_from_file_and_line (file f, diagnostic_line_num_t line_num);

  physical_location
  new_location_from_file_line_column (file f,
				      line_num_t line_num,
				      column_num_t column_num);

  physical_location
  new_location_from_range (physical_location loc_caret,
			   physical_location loc_start,
			   physical_location loc_end);

  void
  debug_dump (physical_location loc,
	      FILE *out);

  logical_location
  new_logical_location (enum diagnostic_logical_location_kind_t kind,
			logical_location parent,
			const char *short_name,
			const char *fully_qualified_name,
			const char *decorated_name);

  void
  debug_dump (logical_location loc,
	      FILE *out);

  execution_path
  new_execution_path ();

  diagnostic
  begin_diagnostic (enum diagnostic_level level);

  void
  set_analysis_target (file f);

  void
  take_global_graph (graph g);

  void
  set_debug_physical_locations (bool value);

  diagnostic_manager *m_inner;
  bool m_owned;
};

class graph
{
public:
  graph () : m_inner (nullptr), m_owned (false) {}

  graph (diagnostic_graph *graph)
  : m_inner (graph), m_owned (true)
  {}

  graph (const diagnostic_graph *graph)
  : m_inner (const_cast<diagnostic_graph *> (graph)),
    m_owned (false)
  {}

  graph (const graph &other) = delete;
  graph &operator= (const graph &other) = delete;

  graph (graph &&other)
  : m_inner (other.m_inner),
    m_owned (other.m_owned)
  {
    other.m_inner = nullptr;
    other.m_owned = false;
  }

  graph &operator= (graph &&other)
  {
    m_inner = other.m_inner;
    m_owned = other.m_owned;
    other.m_inner = nullptr;
    other.m_owned = false;
    return *this;
  }

  ~graph ()
  {
    if (m_owned)
      diagnostic_graph_release (m_inner);
  }

  void
  set_description (const char *);
  void
  set_description (message_buffer &&);

  node
  get_node_by_id (const char *id) const;

  edge
  get_edge_by_id (const char *id) const;

  edge
  add_edge (const char *id, node src_node, node dst_node, const char *label);
  edge
  add_edge (const char *id, node src_node, node dst_node, message_buffer &&label);

  diagnostic_graph *m_inner;
  bool m_owned;
};

// Borrowed pointer to a diagnostic_node.

class node
{
public:
  node () : m_inner (nullptr) {}
  node (diagnostic_node *node_) : m_inner (node_) {}

  void
  set_label (const char *);
  void
  set_label (message_buffer &&);

  void
  set_location (physical_location loc);

  void
  set_logical_location (logical_location loc);

  diagnostic_node *m_inner;
};

// Borrowed edge to a diagnostic_edge.

class edge
{
public:
  edge (diagnostic_edge *edge_) : m_inner (edge_) {}

  diagnostic_edge *m_inner;
};

// Implementation

// class file

inline void
file::set_buffered_content (const char *data, size_t sz)
{
  diagnostic_file_set_buffered_content (m_inner, data, sz);
}

// class physical_location

inline file
physical_location::get_file () const
{
  return file (diagnostic_physical_location_get_file (m_inner));
}

// class logical_location

inline enum diagnostic_logical_location_kind_t
logical_location::get_kind () const
{
  // m_inner must be non-null
  return diagnostic_logical_location_get_kind (m_inner);
}

inline logical_location
logical_location::get_parent () const
{
  if (m_inner)
    return diagnostic_logical_location_get_parent (m_inner);
  else
    return nullptr;
}

inline const char *
logical_location::get_short_name () const
{
  if (m_inner)
    return diagnostic_logical_location_get_short_name (m_inner);
  else
    return nullptr;
}

inline const char *
logical_location::get_fully_qualified_name () const
{
  if (m_inner)
    return diagnostic_logical_location_get_fully_qualified_name (m_inner);
  else
    return nullptr;
}

inline const char *
logical_location::get_decorated_name () const
{
  if (m_inner)
    return diagnostic_logical_location_get_decorated_name (m_inner);
  else
    return nullptr;
}

// class execution_path

inline diagnostic_event_id
execution_path::add_event (physical_location physical_loc,
			   logical_location logical_loc,
			   unsigned stack_depth,
			   const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  diagnostic_event_id result = add_event_va (physical_loc,
					     logical_loc,
					     stack_depth,
					     fmt, &args);
  va_end (args);

  return result;
}

inline diagnostic_event_id
execution_path::add_event_va (physical_location physical_loc,
			      logical_location logical_loc,
			      unsigned stack_depth,
			      const char *fmt,
			      va_list *args)
{
  return diagnostic_execution_path_add_event_va (m_inner,
						 physical_loc.m_inner,
						 logical_loc.m_inner,
						 stack_depth,
						 fmt,
						 args);
}

inline diagnostic_event_id
execution_path::add_event_via_msg_buf (physical_location physical_loc,
				       logical_location logical_loc,
				       unsigned stack_depth,
				       message_buffer &&msg_buf)
{
  diagnostic_message_buffer *inner_msg_buf = msg_buf.m_inner;
  msg_buf.m_inner = nullptr;
  return diagnostic_execution_path_add_event_via_msg_buf (m_inner,
							  physical_loc.m_inner,
							  logical_loc.m_inner,
							  stack_depth,
							  inner_msg_buf);
}

// class group

inline
group::group (manager &mgr)
: m_mgr (mgr)
{
  diagnostic_manager_begin_group (m_mgr.m_inner);
}

inline
group::~group ()
{
  diagnostic_manager_end_group (m_mgr.m_inner);
}

// class diagnostic

inline void
diagnostic::set_cwe (unsigned cwe_id)
{
  diagnostic_set_cwe (m_inner, cwe_id);
}

inline void
diagnostic::add_rule (const char *title, const char *url)
{
  diagnostic_add_rule (m_inner, title, url);
}

inline void
diagnostic::set_location (physical_location loc)
{
  diagnostic_set_location (m_inner, loc.m_inner);
}

inline void
diagnostic::add_location_with_label (physical_location loc,
				     const char *text)
{
  diagnostic_add_location_with_label (m_inner, loc.m_inner, text);
}

inline void
diagnostic::add_location_with_label (physical_location loc,
				     message_buffer &&msg_buf)
{
  diagnostic_message_buffer *inner_msg_buf = msg_buf.m_inner;
  msg_buf.m_inner = nullptr;
  diagnostic_add_location_with_label_via_msg_buf (m_inner,
						  loc.m_inner,
						  inner_msg_buf);
}

inline void
diagnostic::add_location (physical_location loc)
{
  diagnostic_add_location (m_inner, loc.m_inner);
}

inline void
diagnostic::set_logical_location (logical_location loc)
{
  diagnostic_set_logical_location (m_inner, loc.m_inner);
}

inline void
diagnostic::add_fix_it_hint_insert_before (physical_location loc,
					   const char *addition)
{
  diagnostic_add_fix_it_hint_insert_before (m_inner,
					    loc.m_inner,
					    addition);
}

inline void
diagnostic::add_fix_it_hint_insert_after (physical_location loc,
				const char *addition)
{
  diagnostic_add_fix_it_hint_insert_after (m_inner,
					   loc.m_inner,
					   addition);
}

inline void
diagnostic::add_fix_it_hint_replace (physical_location loc,
				     const char *replacement)
{
  diagnostic_add_fix_it_hint_replace (m_inner,
				      loc.m_inner,
				      replacement);
}

inline void
diagnostic::add_fix_it_hint_delete (physical_location loc)
{
  diagnostic_add_fix_it_hint_delete (m_inner,
				     loc.m_inner);
}

inline void
diagnostic::take_execution_path (execution_path path)
{
  diagnostic_take_execution_path (m_inner,
				  path.m_inner);
  path.m_owned = false;
}

inline void
diagnostic::take_graph (graph g)
{
  diagnostic_take_graph (m_inner,
			 g.m_inner);
  g.m_owned = false;
}

inline void
diagnostic::finish (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  diagnostic_finish_va (m_inner, fmt, &ap);
  va_end (ap);
}

inline void
diagnostic::finish_va (const char *fmt, va_list *args)
{
  diagnostic_finish_va (m_inner, fmt, args);
}

inline void
diagnostic::finish_via_msg_buf (message_buffer &&msg_buf)
{
  diagnostic_message_buffer *inner_msg_buf = msg_buf.m_inner;
  msg_buf.m_inner = nullptr;
  diagnostic_finish_via_msg_buf (m_inner, inner_msg_buf);
}

// class manager

inline file
manager::new_file (const char *name,
		   const char *sarif_source_language)
{
  return file
    (diagnostic_manager_new_file (m_inner, name, sarif_source_language));
}

inline physical_location
manager::new_location_from_file_and_line (file f,
					  diagnostic_line_num_t line_num)
{
  return physical_location
    (diagnostic_manager_new_location_from_file_and_line (m_inner,
							 f.m_inner,
							 line_num));
}

inline physical_location
manager::new_location_from_file_line_column (file f,
					     line_num_t line_num,
					     column_num_t column_num)
{
  return physical_location
    (diagnostic_manager_new_location_from_file_line_column (m_inner,
							    f.m_inner,
							    line_num,
							    column_num));
}

inline physical_location
manager::new_location_from_range (physical_location loc_caret,
				  physical_location loc_start,
				  physical_location loc_end)
{
  return physical_location
    (diagnostic_manager_new_location_from_range (m_inner,
						 loc_caret.m_inner,
						 loc_start.m_inner,
						 loc_end.m_inner));
}

inline void
manager::debug_dump (physical_location loc,
		     FILE *out)
{
  diagnostic_manager_debug_dump_location (m_inner,
					  loc.m_inner,
					  out);
}
inline logical_location
manager::new_logical_location (enum diagnostic_logical_location_kind_t kind,
			       logical_location parent,
			       const char *short_name,
			       const char *fully_qualified_name,
			       const char *decorated_name)
{
  return logical_location
    (diagnostic_manager_new_logical_location (m_inner,
					      kind,
					      parent.m_inner,
					      short_name,
					      fully_qualified_name,
					      decorated_name));
}

inline void
manager::debug_dump (logical_location loc,
		     FILE *out)
{
  diagnostic_manager_debug_dump_logical_location (m_inner,
						  loc.m_inner,
						  out);
}

inline execution_path
manager::new_execution_path ()
{
  return execution_path (diagnostic_manager_new_execution_path (m_inner));
}

inline diagnostic
manager::begin_diagnostic (enum diagnostic_level level)
{
  return diagnostic (diagnostic_begin (m_inner, level));
}

inline void
manager::set_analysis_target (file f)
{
  diagnostic_manager_set_analysis_target (m_inner, f.m_inner);
}

inline void
manager::take_global_graph (graph g)
{
  diagnostic_manager_take_global_graph (m_inner,
					g.m_inner);
  g.m_owned = false;
}

inline void
manager::set_debug_physical_locations (bool value)
{
  diagnostic_manager_set_debug_physical_locations (m_inner,
						   value ? 1 : 0);
}

// class graph

inline void
graph::set_description (const char *desc)
{
  diagnostic_graph_set_description (m_inner, desc);
}

inline void
graph::set_description (message_buffer &&msg_buf)
{
  diagnostic_message_buffer *inner_msg_buf = msg_buf.m_inner;
  msg_buf.m_inner = nullptr;
  diagnostic_graph_set_description_via_msg_buf (m_inner, inner_msg_buf);
}

inline node
graph::get_node_by_id (const char *id) const
{
  return node (diagnostic_graph_get_node_by_id (m_inner, id));
}

inline edge
graph::get_edge_by_id (const char *id) const
{
  return edge (diagnostic_graph_get_edge_by_id (m_inner, id));
}

inline edge
graph::add_edge (const char *id,
		 node src_node, node dst_node,
		 const char *label)
{
  return edge (diagnostic_graph_add_edge (m_inner,
					  id,
					  src_node.m_inner,
					  dst_node.m_inner,
					  label));
}

inline edge
graph::add_edge (const char *id,
		 node src_node, node dst_node,
		 message_buffer &&label)
{
  diagnostic_message_buffer *inner_label = label.m_inner;
  label.m_inner = nullptr;
  return edge (diagnostic_graph_add_edge_via_msg_buf (m_inner,
						      id,
						      src_node.m_inner,
						      dst_node.m_inner,
						      inner_label));
}

// class node

inline void
node::set_label (const char *label)
{
  diagnostic_node_set_label (m_inner, label);
}

inline void
node::set_label (message_buffer &&label)
{
  diagnostic_message_buffer *inner_label = label.m_inner;
  label.m_inner = nullptr;
  diagnostic_node_set_label_via_msg_buf (m_inner, inner_label);
}

inline void
node::set_location (physical_location loc)
{
  diagnostic_node_set_location (m_inner, loc.m_inner);
}

inline void
node::set_logical_location (logical_location loc)
{
  diagnostic_node_set_logical_location (m_inner, loc.m_inner);
}

} // namespace libgdiagnostics

#endif // #ifndef LIBGDIAGNOSTICSPP_H
