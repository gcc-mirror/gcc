/* Call stacks at program points.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
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
class supernode;
class call_superedge;
class return_superedge;


/* A string of return_superedge pointers, representing a call stack
   at a program point.

   This is used to ensure that we generate interprocedurally valid paths
   i.e. that we return to the same callsite that called us.

   The class stores returning calls ( which may be represented by a
   returning superedge ). We do so because this is what we need to compare 
   against.  */

class call_string
{
public:
  /* A struct representing an element in the call_string.

   Each element represents a path from m_callee to m_caller which represents
   returning from function.  */

  struct element_t
  {
    element_t (const supernode *caller, const supernode *callee)
    :  m_caller (caller), m_callee (callee)
    {
    }

    bool operator== (const element_t &other) const;
    bool operator!= (const element_t &other) const;

    /* Accessors */
    function *get_caller_function () const;
    function *get_callee_function () const;
    
    const supernode *m_caller;
    const supernode *m_callee;
  };

  call_string () : m_elements () {}
  call_string (const call_string &other);
  call_string& operator= (const call_string &other);

  bool operator== (const call_string &other) const;

  void print (pretty_printer *pp) const;

  json::value *to_json () const;

  hashval_t hash () const;

  bool empty_p () const { return m_elements.is_empty (); }

  void push_call (const supergraph &sg,
		  const call_superedge *sedge);

  void push_call (const supernode *src, 
    const supernode *dest);

  element_t pop ();

  int calc_recursion_depth () const;

  static int cmp (const call_string &a,
		  const call_string &b);

  /* Accessors */

  const supernode *get_callee_node () const;
  const supernode *get_caller_node () const;
  unsigned length () const { return m_elements.length (); }
  element_t operator[] (unsigned idx) const
  {
    return m_elements[idx];
  }

  void validate () const;

private:
  auto_vec<element_t> m_elements;
};

} // namespace ana

#endif /* GCC_ANALYZER_CALL_STRING_H */
