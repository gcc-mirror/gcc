/* Call stacks at program points.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_CALL_STRING_H
#define GCC_ANALYZER_CALL_STRING_H

namespace ana {

class supergraph;
class call_superedge;
class return_superedge;

/* A string of return_superedge pointers, representing a call stack
   at a program point.

   This is used to ensure that we generate interprocedurally valid paths
   i.e. that we return to the same callsite that called us.

   The class actually stores the return edges, rather than the call edges,
   since that's what we need to compare against.  */

class call_string
{
public:
  call_string () : m_return_edges () {}
  call_string (const call_string &other);
  call_string& operator= (const call_string &other);

  bool operator== (const call_string &other) const;

  void print (pretty_printer *pp) const;

  hashval_t hash () const;

  bool empty_p () const { return m_return_edges.is_empty (); }

  void push_call (const supergraph &sg,
		  const call_superedge *sedge);
  const return_superedge *pop () { return m_return_edges.pop (); }

  int calc_recursion_depth () const;

  static int cmp (const call_string &a,
		  const call_string &b);

  unsigned length () const { return m_return_edges.length (); }
  const return_superedge *operator[] (unsigned idx) const
  {
    return m_return_edges[idx];
  }

  void validate () const;

private:
  auto_vec<const return_superedge *> m_return_edges;
};

} // namespace ana

#endif /* GCC_ANALYZER_CALL_STRING_H */
