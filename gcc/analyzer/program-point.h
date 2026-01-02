/* Classes for representing locations within the program.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#ifndef GCC_ANALYZER_PROGRAM_POINT_H
#define GCC_ANALYZER_PROGRAM_POINT_H

#include "pretty-print.h"
#include "analyzer/call-string.h"
#include "analyzer/supergraph.h"

namespace ana {

class format
{
public:
  format (bool newlines) : m_newlines (newlines) {}

  void spacer (pretty_printer *pp) const
  {
    if (m_newlines)
      pp_newline (pp);
    else
      pp_space (pp);
  }

  bool m_newlines;
};

/* A class for representing a location within the program, including
   interprocedural information.

   This represents a fine-grained location within the supergraph (or
   within one of its nodes), along with a call string giving the
   interprocedural context.  */

class program_point
{
public:
  program_point (const supernode *snode,
		 const call_string &call_string)
  : m_snode (snode),
    m_call_string (&call_string)
  {
  }

  void print (pretty_printer *pp, const format &f) const;
  void dump () const;
  void print_source_line (pretty_printer *pp) const;

  std::unique_ptr<json::object> to_json () const;

  hashval_t hash () const;
  bool operator== (const program_point &other) const
  {
    return (m_snode == other.m_snode
	    && m_call_string == other.m_call_string);
  }
  bool operator!= (const program_point &other) const
  {
    return !(*this == other);
  }

  /* Accessors.  */

  const supernode *get_supernode () const
  {
    return m_snode;
  }
  const call_string &get_call_string () const { return *m_call_string; }

  function *get_function () const
  {
    return m_snode ? m_snode->get_function () : nullptr;
  }
  function *get_function_at_depth (unsigned depth) const;
  tree get_fndecl () const
  {
    function *fn = get_function ();
    return fn ? fn->decl : nullptr;
  }
  location_t get_location () const
  {
    return m_snode ? m_snode->get_location () : UNKNOWN_LOCATION;
  }

  /* Get the number of frames we expect at this program point.
     This will be one more than the length of the call_string
     (which stores the parent callsites), apart from the origin
     node, which doesn't have any frames.  */
  int get_stack_depth () const
  {
    if (m_snode == nullptr)
      // Origin
      return 0;
    return get_call_string ().length () + 1;
  }

  bool state_merge_at_p () const
  {
    if (m_snode)
      return m_snode->m_state_merger_node;
    return false;
  }

  /* Factory functions for making various kinds of program_point.  */
  static program_point origin (const region_model_manager &mgr);
  static program_point from_function_entry (const region_model_manager &mgr,
					    const supergraph &sg,
					    const function &fun);

  void pop_from_call_stack ();
  void validate () const;

  static bool effectively_intraprocedural_p (const program_point &point_a,
					     const program_point &point_b);

 private:
  const supernode *m_snode;
  const call_string *m_call_string;
};

} // namespace ana

#endif /* GCC_ANALYZER_PROGRAM_POINT_H */
