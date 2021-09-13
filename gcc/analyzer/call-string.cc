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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "pretty-print.h"
#include "tree.h"
#include "options.h"
#include "json.h"
#include "analyzer/call-string.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "function.h"
#include "cfg.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "digraph.h"
#include "analyzer/supergraph.h"

#if ENABLE_ANALYZER

#if __GNUC__ >= 10
#pragma GCC diagnostic ignored "-Wformat-diag"
#endif

/* class call_string.  */

/* struct call_string::element_t.  */

/* call_string::element_t's equality operator.  */

bool
call_string::element_t::operator== (const call_string::element_t &other) const
{
  return (m_caller == other.m_caller && m_callee == other.m_callee);
}

/* call_string::element_t's inequality operator.  */
bool
call_string::element_t::operator!= (const call_string::element_t &other) const
{
  return !(*this == other);
}

function *
call_string::element_t::get_caller_function () const
{
  return m_caller->get_function ();
}

function *
call_string::element_t::get_callee_function () const
{
  return m_callee->get_function ();
}

/* call_string's copy ctor.  */

call_string::call_string (const call_string &other)
: m_elements (other.m_elements.length ())
{
  for (const call_string::element_t &e : other.m_elements)
    m_elements.quick_push (e);
}

/* call_string's assignment operator.  */

call_string&
call_string::operator= (const call_string &other)
{
  // would be much simpler if we could rely on vec<> assignment op
  m_elements.truncate (0);
  m_elements.reserve (other.m_elements.length (), true);
  call_string::element_t *e;
  int i;
  FOR_EACH_VEC_ELT (other.m_elements, i, e)
    m_elements.quick_push (*e);
  return *this;
}

/* call_string's equality operator.  */

bool
call_string::operator== (const call_string &other) const
{
  if (m_elements.length () != other.m_elements.length ())
    return false;
  call_string::element_t *e;
  int i;
  FOR_EACH_VEC_ELT (m_elements, i, e)
    if (*e != other.m_elements[i])
      return false;
  return true;
}

/* Print this to PP.  */

void
call_string::print (pretty_printer *pp) const
{
  pp_string (pp, "[");

  call_string::element_t *e;
  int i;
  FOR_EACH_VEC_ELT (m_elements, i, e)
    {
      if (i > 0)
	pp_string (pp, ", ");
      pp_printf (pp, "(SN: %i -> SN: %i in %s)",
		 e->m_callee->m_index, e->m_caller->m_index,
		 function_name (e->m_caller->m_fun));
    }

  pp_string (pp, "]");
}

/* Return a new json::array of the form
   [{"src_snode_idx" : int,
     "dst_snode_idx" : int,
     "funcname" : str},
     ...for each element in the callstring].  */

json::value *
call_string::to_json () const
{
  json::array *arr = new json::array ();

  for (const call_string::element_t &e : m_elements)
    {
      json::object *e_obj = new json::object ();
      e_obj->set ("src_snode_idx",
		  new json::integer_number (e.m_callee->m_index));
      e_obj->set ("dst_snode_idx",
		  new json::integer_number (e.m_caller->m_index));
      e_obj->set ("funcname",
		  new json::string (function_name (e.m_caller->m_fun)));
      arr->append (e_obj);
    }

  return arr;
}

/* Generate a hash value for this call_string.  */

hashval_t
call_string::hash () const
{
  inchash::hash hstate;
  for (const call_string::element_t &e : m_elements)
    hstate.add_ptr (e.m_caller);
  return hstate.end ();
}

/* Push the return superedge for CALL_SEDGE onto the end of this
   call_string.  */

void
call_string::push_call (const supergraph &sg,
			const call_superedge *call_sedge)
{
  gcc_assert (call_sedge);
  const return_superedge *return_sedge = call_sedge->get_edge_for_return (sg);
  gcc_assert (return_sedge);
  call_string::element_t e (return_sedge->m_dest, return_sedge->m_src);
  m_elements.safe_push (e);
}

void
call_string::push_call (const supernode *caller,
			const supernode *callee)
{
  call_string::element_t e (caller, callee);
  m_elements.safe_push (e);
}

call_string::element_t
call_string::pop ()
{
  return m_elements.pop();
}

/* Count the number of times the top-most call site appears in the
   stack.  */
int
call_string::calc_recursion_depth () const
{
  if (m_elements.is_empty ())
    return 0;
  const call_string::element_t top_return_sedge 
    = m_elements[m_elements.length () - 1];

  int result = 0;
  for (const call_string::element_t &e : m_elements)
    if (e == top_return_sedge)
      ++result;
  return result;
}

/* Comparator for call strings.
   This implements a version of lexicographical order.
   Return negative if A is before B.
   Return positive if B is after A.
   Return 0 if they are equal.  */

int
call_string::cmp (const call_string &a,
		  const call_string &b)
{
  unsigned len_a = a.length ();
  unsigned len_b = b.length ();

  unsigned i = 0;
  while (1)
    {
      /* Consider index i; the strings have been equal up to it.  */

      /* Have both strings run out?  */
      if (i >= len_a && i >= len_b)
	return 0;

      /* Otherwise, has just one of the strings run out?  */
      if (i >= len_a)
	return 1;
      if (i >= len_b)
	return -1;

      /* Otherwise, compare the node pairs.  */
      const call_string::element_t a_node_pair = a[i];
      const call_string::element_t b_node_pair = b[i];
      int src_cmp 
      	= a_node_pair.m_callee->m_index - b_node_pair.m_callee->m_index;
      if (src_cmp)
	return src_cmp;
      int dest_cmp 
      	= a_node_pair.m_caller->m_index - b_node_pair.m_caller->m_index;
      if (dest_cmp)
	return dest_cmp;
      i++;
      // TODO: test coverage for this
    }
}

/* Return the pointer to callee of the topmost call in the stack,
   or NULL if stack is empty.  */
const supernode *
call_string::get_callee_node () const
{
  if(m_elements.is_empty ())
    return NULL;
  return m_elements[m_elements.length () - 1].m_callee;
}

/* Return the pointer to caller of the topmost call in the stack,
   or NULL if stack is empty.  */
const supernode * 
call_string::get_caller_node () const
{
  if(m_elements.is_empty ())
    return NULL;
  return m_elements[m_elements.length () - 1].m_caller;
}

/* Assert that this object is sane.  */

void
call_string::validate () const
{
  /* Skip this in a release build.  */
#if !CHECKING_P
  return;
#endif

  /* Each entry's "caller" should be the "callee" of the previous entry.  */
  call_string::element_t *e;
  int i;
  FOR_EACH_VEC_ELT (m_elements, i, e)
    if (i > 0)
    {
      gcc_assert (e->get_caller_function () == 
      		  m_elements[i - 1].get_callee_function ());
    }
}

#endif /* #if ENABLE_ANALYZER */
