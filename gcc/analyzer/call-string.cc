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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "pretty-print.h"
#include "tree.h"
#include "options.h"
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

/* class call_string.  */

/* call_string's copy ctor.  */

call_string::call_string (const call_string &other)
: m_return_edges (other.m_return_edges.length ())
{
  const return_superedge *e;
  int i;
  FOR_EACH_VEC_ELT (other.m_return_edges, i, e)
    m_return_edges.quick_push (e);
}

/* call_string's assignment operator.  */

call_string&
call_string::operator= (const call_string &other)
{
  // would be much simpler if we could rely on vec<> assignment op
  m_return_edges.truncate (0);
  m_return_edges.reserve (other.m_return_edges.length (), true);
  const return_superedge *e;
  int i;
  FOR_EACH_VEC_ELT (other.m_return_edges, i, e)
    m_return_edges.quick_push (e);
  return *this;
}

/* call_string's equality operator.  */

bool
call_string::operator== (const call_string &other) const
{
  if (m_return_edges.length () != other.m_return_edges.length ())
    return false;
  const return_superedge *e;
  int i;
  FOR_EACH_VEC_ELT (m_return_edges, i, e)
    if (e != other.m_return_edges[i])
      return false;
  return true;
}

/* Print this to PP.  */

void
call_string::print (pretty_printer *pp) const
{
  pp_string (pp, "[");

  const return_superedge *e;
  int i;
  FOR_EACH_VEC_ELT (m_return_edges, i, e)
    {
      if (i > 0)
	pp_string (pp, ", ");
      pp_printf (pp, "(SN: %i -> SN: %i in %s)",
		 e->m_src->m_index, e->m_dest->m_index,
		 function_name (e->m_dest->m_fun));
    }

  pp_string (pp, "]");
}

/* Generate a hash value for this call_string.  */

hashval_t
call_string::hash () const
{
  inchash::hash hstate;
  int i;
  const return_superedge *e;
  FOR_EACH_VEC_ELT (m_return_edges, i, e)
    hstate.add_ptr (e);
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
  m_return_edges.safe_push (return_sedge);
}

/* Count the number of times the top-most call site appears in the
   stack.  */

int
call_string::calc_recursion_depth () const
{
  if (m_return_edges.is_empty ())
    return 0;
  const return_superedge *top_return_sedge
    = m_return_edges[m_return_edges.length () - 1];

  int result = 0;
  const return_superedge *e;
  int i;
  FOR_EACH_VEC_ELT (m_return_edges, i, e)
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

      /* Otherwise, compare the edges.  */
      const return_superedge *edge_a = a[i];
      const return_superedge *edge_b = b[i];
      int src_cmp = edge_a->m_src->m_index - edge_b->m_src->m_index;
      if (src_cmp)
	return src_cmp;
      int dest_cmp = edge_a->m_dest->m_index - edge_b->m_dest->m_index;
      if (dest_cmp)
	return dest_cmp;
      i++;
      // TODO: test coverage for this
    }
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
  const return_superedge *e;
  int i;
  FOR_EACH_VEC_ELT (m_return_edges, i, e)
    if (i > 0)
      gcc_assert (e->get_caller_function ()
		  == m_return_edges[i - 1]->get_callee_function ());
}

#endif /* #if ENABLE_ANALYZER */
