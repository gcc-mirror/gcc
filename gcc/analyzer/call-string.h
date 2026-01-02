/* Call stacks at program points.
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

#ifndef GCC_ANALYZER_CALL_STRING_H
#define GCC_ANALYZER_CALL_STRING_H

namespace ana {

class supergraph;
class supernode;
  class call_and_return_op;

/* A string of "elements" representing a call stack at a program point.

   This is used to ensure that we generate interprocedurally valid paths
   i.e. that we return to the same callsite that called us.

   Instances of call_string are consolidated by the region_model_manager,
   which effectively owns them: it owns the root/empty call_string, and each
   call_string instance tracks its children, lazily creating them on demand,
   so that the call_string instances form a tree-like hierarchy in memory.  */

class call_string
{
public:
  /* A struct representing an element in the call_string.

     Each element represents a "call superedge" within the caller for which
     the op was a call_and_return_op.
     Returning from the callee to the caller will involve creating a custom
     exploded edge from the exit supernode in the callee to the destination
     of the call superedge within the caller.  */

  struct element_t
  {
    element_t (const superedge *call_sedge,
	       const call_and_return_op *call_op,
	       function *called_fun)
    :  m_call_sedge (call_sedge), m_call_op (call_op),
       m_called_fun (called_fun)
    {
    }

    bool operator== (const element_t &other) const;
    bool operator!= (const element_t &other) const;

    static int cmp (const element_t &a, const element_t &b);

    /* Accessors */
    function *get_caller_function () const;
    function *get_callee_function () const { return m_called_fun; }
    const supernode *get_call_snode_in_caller () const;
    const supernode *get_return_snode_in_caller () const;
    const gcall &get_call_stmt () const;

    const superedge *m_call_sedge;
    const call_and_return_op *m_call_op;
    function *m_called_fun;
  };

  void print (pretty_printer *pp) const;

  std::unique_ptr<json::value> to_json () const;

  bool empty_p () const { return m_elements.is_empty (); }

  const call_string *push_call (const superedge &call_sedge,
				const call_and_return_op &call_op,
				function &called_fun) const;
  const call_string *get_parent () const { return m_parent; }

  int calc_recursion_depth () const;

  static int cmp (const call_string &a,
		  const call_string &b);

  static int cmp_ptr_ptr (const void *, const void *);

  /* Accessors */
  const supernode *get_return_node_in_caller () const;
  unsigned length () const { return m_elements.length (); }
  element_t operator[] (unsigned idx) const
  {
    return m_elements[idx];
  }
  const element_t &get_top_of_stack () const
  {
    gcc_assert (m_elements.length () > 0);
    return m_elements[m_elements.length () - 1];
  }

  int count_occurrences_of_function (function *) const;

  void validate () const;

private:
  struct hashmap_traits_t
  {
    typedef element_t key_type;
    typedef const call_string *value_type;

    static const bool maybe_mx = false;
    static inline hashval_t hash (const key_type &k)
    {
      inchash::hash hstate;
      hstate.add_ptr (k.m_call_sedge);
      return hstate.end ();
    }
    static inline bool equal_keys (const key_type &k1, const key_type &k2)
    {
      return k1 == k2;
    }
    template <typename T> static inline void remove (T &entry)
    {
      entry.m_key = element_t (nullptr, nullptr, nullptr);
    }
    static const bool empty_zero_p = true;
    template <typename T> static inline bool is_empty (const T &entry)
    {
      return entry.m_key.m_call_sedge == nullptr;
    }
    template <typename T> static inline bool is_deleted (const T &entry)
    {
      return entry.m_key.m_call_sedge == reinterpret_cast<const superedge *> (1);
    }
    template <typename T> static inline void mark_empty (T &entry)
    {
      entry.m_key = element_t (nullptr, nullptr, nullptr);
      entry.m_value = nullptr;
    }
    template <typename T> static inline void mark_deleted (T &entry)
    {
       entry.m_key.m_call_sedge = reinterpret_cast<const superedge *> (1);
    }
  };

  friend class region_model_manager;

  DISABLE_COPY_AND_ASSIGN (call_string);

  call_string ();
  call_string (const call_string &parent, const element_t &to_push);
  ~call_string ();

  void recursive_log (logger *logger) const;

  const call_string *m_parent;
  auto_vec<element_t> m_elements;
  mutable hash_map<element_t, const call_string *, hashmap_traits_t> m_children;
};

} // namespace ana

#endif /* GCC_ANALYZER_CALL_STRING_H */
