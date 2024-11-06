/* Classes for representing locations within the program.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

namespace ana {

class exploded_graph;

/* An enum for distinguishing the various kinds of program_point.  */

enum point_kind {
  /* A "fake" node which has edges to all entrypoints.  */
  PK_ORIGIN,

  PK_BEFORE_SUPERNODE,
  PK_BEFORE_STMT,
  PK_AFTER_SUPERNODE,

  /* Special values used for hash_map:  */
  PK_EMPTY,
  PK_DELETED,

  NUM_POINT_KINDS
};

extern const char *point_kind_to_string (enum point_kind pk);

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

/* A class for representing a location within the program, without
   interprocedural information.

   This represents a fine-grained location within the supergraph (or
   within one of its nodes).  */

class function_point
{
public:
  function_point (const supernode *supernode,
		  const superedge *from_edge,
		  unsigned stmt_idx,
		  enum point_kind kind);

  void print (pretty_printer *pp, const format &f) const;
  void print_source_line (pretty_printer *pp) const;
  void dump () const;

  hashval_t hash () const;
  bool operator== (const function_point &other) const
  {
    return (m_supernode == other.m_supernode
	    && m_from_edge == other.m_from_edge
	    && m_stmt_idx == other.m_stmt_idx
	    && m_kind == other.m_kind);
  }

  /* Accessors.  */

  const supernode *get_supernode () const { return m_supernode; }
  function *get_function () const;
  const gimple *get_stmt () const;
  location_t get_location () const;
  enum point_kind get_kind () const { return m_kind; }
  const superedge *get_from_edge () const
  {
    return m_from_edge;
  }
  unsigned get_stmt_idx () const
  {
    gcc_assert (m_kind == PK_BEFORE_STMT);
    return m_stmt_idx;
  }

  bool final_stmt_p () const;

  /* Factory functions for making various kinds of program_point.  */

  static function_point from_function_entry (const supergraph &sg,
					     const function &fun);

  static function_point before_supernode (const supernode *supernode,
					  const superedge *from_edge);

  static function_point before_stmt (const supernode *supernode,
				     unsigned stmt_idx)
  {
    return function_point (supernode, NULL, stmt_idx, PK_BEFORE_STMT);
  }

  static function_point after_supernode (const supernode *supernode)
  {
    return function_point (supernode, NULL, 0, PK_AFTER_SUPERNODE);
  }

  /* Support for hash_map.  */

  static function_point empty ()
  {
    return function_point (NULL, NULL, 0, PK_EMPTY);
  }
  static function_point deleted ()
  {
    return function_point (NULL, NULL, 0, PK_DELETED);
  }

  static int cmp_within_supernode_1 (const function_point &point_a,
				     const function_point &point_b);
  static int cmp_within_supernode (const function_point &point_a,
				   const function_point &point_b);
  static int cmp (const function_point &point_a,
		  const function_point &point_b);
  static int cmp_ptr (const void *p1, const void *p2);

  /* For before_stmt, go to next stmt.  */
  void next_stmt ();

  function_point get_next () const;

 private:
  const supernode *m_supernode;

  /* For PK_BEFORE_SUPERNODE, and only for CFG edges.  */
  const superedge *m_from_edge;

  /* Only for PK_BEFORE_STMT.  */
  unsigned m_stmt_idx;

  enum point_kind m_kind;
};

/* A class for representing a location within the program, including
   interprocedural information.

   This represents a fine-grained location within the supergraph (or
   within one of its nodes), along with a call string giving the
   interprocedural context.  */

class program_point
{
public:
  program_point (const function_point &fn_point,
		 const call_string &call_string)
  : m_function_point (fn_point),
    m_call_string (&call_string)
  {
  }

  void print (pretty_printer *pp, const format &f) const;
  void dump () const;

  std::unique_ptr<json::object> to_json () const;

  hashval_t hash () const;
  bool operator== (const program_point &other) const
  {
    return (m_function_point == other.m_function_point
	    && m_call_string == other.m_call_string);
  }
  bool operator!= (const program_point &other) const
  {
    return !(*this == other);
  }

  /* Accessors.  */

  const function_point &get_function_point () const { return m_function_point; }
  const call_string &get_call_string () const { return *m_call_string; }

  const supernode *get_supernode () const
  {
    return m_function_point.get_supernode ();
  }
  function *get_function () const
  {
    return m_function_point.get_function ();
  }
  function *get_function_at_depth (unsigned depth) const;
  tree get_fndecl () const
  {
    gcc_assert (get_kind () != PK_ORIGIN);
    return get_function ()->decl;
  }
  const gimple *get_stmt () const
  {
    return m_function_point.get_stmt ();
  }
  location_t get_location () const
  {
    return m_function_point.get_location ();
  }
  enum point_kind get_kind () const
  {
    return m_function_point.get_kind ();
  }
  const superedge *get_from_edge () const
  {
    return m_function_point.get_from_edge ();
  }
  unsigned get_stmt_idx () const
  {
    return m_function_point.get_stmt_idx ();
  }

  /* Get the number of frames we expect at this program point.
     This will be one more than the length of the call_string
     (which stores the parent callsites), apart from the origin
     node, which doesn't have any frames.  */
  int get_stack_depth () const
  {
    if (get_kind () == PK_ORIGIN)
      return 0;
    return get_call_string ().length () + 1;
  }

  /* Factory functions for making various kinds of program_point.  */
  static program_point origin (const region_model_manager &mgr);
  static program_point from_function_entry (const region_model_manager &mgr,
					    const supergraph &sg,
					    const function &fun);

  static program_point before_supernode (const supernode *supernode,
					 const superedge *from_edge,
					 const call_string &call_string)
  {
    return program_point (function_point::before_supernode (supernode,
							    from_edge),
			  call_string);
  }

  static program_point before_stmt (const supernode *supernode,
				    unsigned stmt_idx,
				    const call_string &call_string)
  {
    return program_point (function_point::before_stmt (supernode, stmt_idx),
			  call_string);
  }

  static program_point after_supernode (const supernode *supernode,
					const call_string &call_string)
  {
    return program_point (function_point::after_supernode (supernode),
			  call_string);
  }

  /* Support for hash_map.  */

  static program_point empty ()
  {
    return program_point (function_point::empty ());
  }
  static program_point deleted ()
  {
    return program_point (function_point::deleted ());
  }

  bool on_edge (exploded_graph &eg, const superedge *succ);
  void push_to_call_stack (const supernode *caller, const supernode *callee);
  void pop_from_call_stack ();
  void validate () const;

  /* For before_stmt, go to next stmt.  */
  void next_stmt () { m_function_point.next_stmt (); }

  program_point get_next () const;

  static bool effectively_intraprocedural_p (const program_point &point_a,
					     const program_point &point_b);

 private:
  program_point (const function_point &fn_point)
  : m_function_point (fn_point),
    m_call_string (NULL)
  {
  }

  function_point m_function_point;
  const call_string *m_call_string;
};

} // namespace ana

#endif /* GCC_ANALYZER_PROGRAM_POINT_H */
