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

#include "analyzer/common.h"

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
  return (m_call_sedge == other.m_call_sedge
	  && m_called_fun == other.m_called_fun);
}

/* call_string::element_t's inequality operator.  */
bool
call_string::element_t::operator!= (const call_string::element_t &other) const
{
  return !(*this == other);
}

int
call_string::element_t::cmp (const element_t &a, const element_t &b)
{
  if (int src_id_cmp = (a.m_call_sedge->m_src->m_id
			- b.m_call_sedge->m_src->m_id))
    return src_id_cmp;

  /* We don't expect multiple call sedges with the same src.  */
  gcc_assert (a.m_call_sedge->m_dest == b.m_call_sedge->m_dest);

  return a.m_called_fun->funcdef_no - b.m_called_fun->funcdef_no;
}

function *
call_string::element_t::get_caller_function () const
{
  return m_call_sedge->m_src->get_function ();
}

const supernode *
call_string::element_t::get_call_snode_in_caller () const
{
  return m_call_sedge->m_src;
}

const supernode *
call_string::element_t::get_return_snode_in_caller () const
{
  return m_call_sedge->m_dest;
}

const gcall &
call_string::element_t::get_call_stmt () const
{
  return m_call_op->get_gcall ();
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
		 e->get_call_snode_in_caller ()->m_id,
		 e->get_return_snode_in_caller ()->m_id,
		 function_name (e->get_caller_function ()));
    }

  pp_string (pp, "]");
}

/* Return a new json::array of the form
   [{"call_sedge_src" : int,
     "call_sedge_dest" : int,
     "called_fun" : str},
     ...for each element in the callstring].  */

std::unique_ptr<json::value>
call_string::to_json () const
{
  auto arr = std::make_unique<json::array> ();

  for (const call_string::element_t &e : m_elements)
    {
      auto e_obj = std::make_unique<json::object> ();
      e_obj->set_integer ("call_sedge_src", e.m_call_sedge->m_src->m_id);
      e_obj->set_integer ("call_sedge_dest", e.m_call_sedge->m_dest->m_id);
      e_obj->set_string ("called_fun", function_name (e.m_called_fun));
      arr->append (std::move (e_obj));
    }

  return arr;
}


/* Get or create the call_string resulting from pushing the call
   (caller, callee) onto the end of this call_string.  */

const call_string *
call_string::push_call (const superedge &call_sedge,
			const call_and_return_op &call_op,
			function &called_fun) const
{
  call_string::element_t e (&call_sedge, &call_op, &called_fun);

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
      if (e.get_caller_function () == fun)
	result++;
      if (e.get_callee_function () == fun)
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

      /* Otherwise, compare the elements.  */
      if (int element_cmp = call_string::element_t::cmp (a[i], b[i]))
	return element_cmp;
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

/* Return the pointer to caller of the topmost call in the stack,
   or nullptr if stack is empty.  */
const supernode *
call_string::get_return_node_in_caller () const
{
  if(m_elements.is_empty ())
    return nullptr;
  return m_elements[m_elements.length () - 1].get_return_snode_in_caller ();
}

/* Assert that this object is sane.  */

void
call_string::validate () const
{
  /* Skip this in a release build.  */
#if !CHECKING_P
  return;
#endif

  gcc_assert ((m_parent != nullptr)
	      ^ (m_elements.length () == 0));

  call_string::element_t *e;
  int i;
  FOR_EACH_VEC_ELT (m_elements, i, e)
    {
      gcc_assert (e->m_call_op == e->m_call_sedge->get_op ());
      /* Each entry's "caller" should be the "callee" of the
	 previous entry.  */
      if (i > 0)
	gcc_assert (e->get_caller_function () ==
		    m_elements[i - 1].get_callee_function ());
    }
}

/* ctor for the root/empty call_string.  */

call_string::call_string ()
: m_parent (nullptr), m_elements ()
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
		 e->m_call_sedge->m_src->m_id,
		 e->m_call_sedge->m_dest->m_id,
		 function_name (e->get_caller_function ()));
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
