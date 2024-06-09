/* Call stacks at program points.
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

#include "config.h"
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "pretty-print.h"
#include "tree.h"
#include "options.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "function.h"
#include "cfg.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "digraph.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/call-string.h"
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

/* Get or create the call_string resulting from pushing the return
   superedge for CALL_SEDGE onto the end of this call_string.  */

const call_string *
call_string::push_call (const supergraph &sg,
			const call_superedge *call_sedge) const
{
  gcc_assert (call_sedge);
  const return_superedge *return_sedge = call_sedge->get_edge_for_return (sg);
  gcc_assert (return_sedge);
  return push_call (return_sedge->m_dest, return_sedge->m_src);
}

/* Get or create the call_string resulting from pushing the call
   (caller, callee) onto the end of this call_string.  */

const call_string *
call_string::push_call (const supernode *caller,
			const supernode *callee) const
{
  call_string::element_t e (caller, callee);

  if (const call_string **slot = m_children.get (e))
    return *slot;

  call_string *result = new call_string (*this, e);
  m_children.put (e, result);
  return result;
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

/* Count the number of times FUN appears in the string.  */

int
call_string::count_occurrences_of_function (function *fun) const
{
  int result = 0;
  for (const call_string::element_t &e : m_elements)
    {
      if (e.get_callee_function () == fun)
	result++;
      if (e.get_caller_function () == fun)
	result++;
    }
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

/* Comparator for use by vec<const call_string *>::qsort.  */

int
call_string::cmp_ptr_ptr (const void *pa, const void *pb)
{
  const call_string *cs_a = *static_cast <const call_string * const *> (pa);
  const call_string *cs_b = *static_cast <const call_string * const *> (pb);
  return cmp (*cs_a, *cs_b);
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

  gcc_assert (m_parent || m_elements.length () == 0);

  /* Each entry's "caller" should be the "callee" of the previous entry.  */
  call_string::element_t *e;
  int i;
  FOR_EACH_VEC_ELT (m_elements, i, e)
    if (i > 0)
      gcc_assert (e->get_caller_function () ==
		  m_elements[i - 1].get_callee_function ());
}

/* ctor for the root/empty call_string.  */

call_string::call_string ()
: m_parent (NULL), m_elements ()
{
}

/* ctor for a child call_string.  */

call_string::call_string (const call_string &parent, const element_t &to_push)
: m_parent (&parent),
  m_elements (parent.m_elements.length () + 1)
{
  m_elements.splice (parent.m_elements);
  m_elements.quick_push (to_push);
}

/* dtor for call_string: recursively delete children.  */

call_string::~call_string ()
{
  for (auto child_iter : m_children)
    delete child_iter.second;
}

/* Log this call_string and all its descendents recursively to LOGGER,
   using indentation and elision to highlight the hierarchy.  */

void
call_string::recursive_log (logger *logger) const
{
  logger->start_log_line ();
  pretty_printer *pp = logger->get_printer ();
  for (unsigned i = 0; i < length (); i++)
    pp_string (pp, "  ");
  if (length () > 0)
    {
      pp_string (pp, "[");
      /* Elide all but the final element, since they are shared with
	 the parent call_string.  */
      for (unsigned i = 0; i < length (); i++)
	pp_string (pp, "..., ");
      /* Log the final element in detail.  */
      const element_t *e = &m_elements[m_elements.length () - 1];
      pp_printf (pp, "(SN: %i -> SN: %i in %s)]",
		 e->m_callee->m_index, e->m_caller->m_index,
		 function_name (e->m_caller->m_fun));
    }
  else
    pp_string (pp, "[]");
  logger->end_log_line ();

  /* Recurse into children.  */
  {
    auto_vec<const call_string *> children (m_children.elements ());
    for (auto iter : m_children)
      children.safe_push (iter.second);
    children.qsort (call_string::cmp_ptr_ptr);

    for (auto iter : children)
      iter->recursive_log (logger);
  }
}

#endif /* #if ENABLE_ANALYZER */
