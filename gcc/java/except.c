/* Handle exceptions for GNU compiler for the Java(TM) language.
   Copyright (C) 1997, 1998, 1999, 2000, 2002 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "real.h"
#include "rtl.h"
#include "java-tree.h"
#include "javaop.h"
#include "java-opcodes.h"
#include "jcf.h"
#include "function.h"
#include "except.h"
#include "java-except.h"
#include "toplev.h"

static void expand_start_java_handler PARAMS ((struct eh_range *));
static void expand_end_java_handler PARAMS ((struct eh_range *));
static struct eh_range *find_handler_in_range PARAMS ((int, struct eh_range *,
						      struct eh_range *));
static void link_handler PARAMS ((struct eh_range *, struct eh_range *));
static void check_start_handlers PARAMS ((struct eh_range *, int));
static void free_eh_ranges PARAMS ((struct eh_range *range));

struct eh_range *current_method_handlers;

struct eh_range *current_try_block = NULL;

struct eh_range *eh_range_freelist = NULL;

/* These variables are used to speed up find_handler. */

static int cache_range_start, cache_range_end;
static struct eh_range *cache_range;
static struct eh_range *cache_next_child;

/* A dummy range that represents the entire method. */

struct eh_range whole_range;

#if defined(DEBUG_JAVA_BINDING_LEVELS)
extern int binding_depth;
extern int is_class_level;
extern int current_pc;
extern void indent ();

#endif

/* Search for the most specific eh_range containing PC.
   Assume PC is within RANGE.
   CHILD is a list of children of RANGE such that any
   previous children have end_pc values that are too low. */

static struct eh_range *
find_handler_in_range (pc, range, child)
     int pc;
     struct eh_range *range;
     register struct eh_range *child;
{
  for (; child != NULL;  child = child->next_sibling)
    {
      if (pc < child->start_pc)
	break;
      if (pc < child->end_pc)
	return find_handler_in_range (pc, child, child->first_child);
    }
  cache_range = range;
  cache_range_start = pc;
  cache_next_child = child;
  cache_range_end = child == NULL ? range->end_pc : child->start_pc;
  return range;
}

/* Find the inner-most handler that contains PC. */

struct eh_range *
find_handler (pc)
     int pc;
{
  struct eh_range *h;
  if (pc >= cache_range_start)
    {
      h = cache_range;
      if (pc < cache_range_end)
	return h;
      while (pc >= h->end_pc)
	{
	  cache_next_child = h->next_sibling;
	  h = h->outer;
	}
    }
  else
    {
      h = &whole_range;
      cache_next_child = h->first_child;
    }
  return find_handler_in_range (pc, h, cache_next_child);
}

/* Recursive helper routine for check_nested_ranges. */

static void
link_handler (range, outer)
     struct eh_range *range, *outer;
{
  struct eh_range **ptr;

  if (range->start_pc == outer->start_pc && range->end_pc == outer->end_pc)
    {
      outer->handlers = chainon (outer->handlers, range->handlers);
      return;
    }

  /* If the new range completely encloses the `outer' range, then insert it
     between the outer range and its parent.  */
  if (range->start_pc <= outer->start_pc && range->end_pc >= outer->end_pc)
    {
      range->outer = outer->outer;
      range->next_sibling = NULL;
      range->first_child = outer;
      {
	struct eh_range **pr = &(outer->outer->first_child);
	while (*pr != outer)
	  pr = &(*pr)->next_sibling;
	*pr = range;
      }
      outer->outer = range;
      return;
    }

  /* Handle overlapping ranges by splitting the new range.  */
  if (range->start_pc < outer->start_pc || range->end_pc > outer->end_pc)
    {
      struct eh_range *h = xmalloc (sizeof (struct eh_range));
      if (range->start_pc < outer->start_pc)
	{
	  h->start_pc = range->start_pc;
	  h->end_pc = outer->start_pc;
	  range->start_pc = outer->start_pc;
	}
      else
	{
	  h->start_pc = outer->end_pc;
	  h->end_pc = range->end_pc;
	  range->end_pc = outer->end_pc;
	}
      h->first_child = NULL;
      h->outer = NULL;
      h->handlers = build_tree_list (TREE_PURPOSE (range->handlers),
				     TREE_VALUE (range->handlers));
      h->next_sibling = NULL;
      h->expanded = 0;
      /* Restart both from the top to avoid having to make this
	 function smart about reentrancy.  */
      link_handler (h, &whole_range);
      link_handler (range, &whole_range);
      return;
    }

  ptr = &outer->first_child;
  for (;; ptr = &(*ptr)->next_sibling)
    {
      if (*ptr == NULL || range->end_pc <= (*ptr)->start_pc)
	{
	  range->next_sibling = *ptr;
	  range->first_child = NULL;
	  range->outer = outer;
	  *ptr = range;
	  return;
	}
      else if (range->start_pc < (*ptr)->end_pc)
	{
	  link_handler (range, *ptr);
	  return;
	}
      /* end_pc > (*ptr)->start_pc && start_pc >= (*ptr)->end_pc. */
    }
}

/* The first pass of exception range processing (calling add_handler)
   constructs a linked list of exception ranges.  We turn this into
   the data structure expected by the rest of the code, and also
   ensure that exception ranges are properly nested.  */

void
handle_nested_ranges ()
{
  struct eh_range *ptr, *next;

  ptr = whole_range.first_child;
  whole_range.first_child = NULL;
  for (; ptr; ptr = next)
    {
      next = ptr->next_sibling;
      ptr->next_sibling = NULL;
      link_handler (ptr, &whole_range);
    }
}

/* Free RANGE as well as its children and siblings.  */

static void
free_eh_ranges (range)
     struct eh_range *range;
{
  while (range) 
    {
      struct eh_range *next = range->next_sibling;
      free_eh_ranges (range->first_child);
      if (range != &whole_range)
	free (range);
      range = next;
    }
}

/* Called to re-initialize the exception machinery for a new method. */

void
method_init_exceptions ()
{
  free_eh_ranges (&whole_range);
  whole_range.start_pc = 0;
  whole_range.end_pc = DECL_CODE_LENGTH (current_function_decl) + 1;
  whole_range.outer = NULL;
  whole_range.first_child = NULL;
  whole_range.next_sibling = NULL;
  cache_range_start = 0xFFFFFF;
}

/* Add an exception range.  If we already have an exception range
   which has the same handler and label, and the new range overlaps
   that one, then we simply extend the existing range.  Some bytecode
   obfuscators generate seemingly nonoverlapping exception ranges
   which, when coalesced, do in fact nest correctly.
   
   This constructs an ordinary linked list which check_nested_ranges()
   later turns into the data structure we actually want.
   
   We expect the input to come in order of increasing START_PC.  This
   function doesn't attempt to detect the case where two previously
   added disjoint ranges could be coalesced by a new range; that is
   what the sorting counteracts.  */

void
add_handler (start_pc, end_pc, handler, type)
     int start_pc, end_pc;
     tree handler;
     tree type;
{
  struct eh_range *ptr, *prev = NULL, *h;

  for (ptr = whole_range.first_child; ptr; ptr = ptr->next_sibling)
    {
      if (start_pc >= ptr->start_pc
	  && start_pc <= ptr->end_pc
	  && TREE_PURPOSE (ptr->handlers) == type
	  && TREE_VALUE (ptr->handlers) == handler)
	{
	  /* Already found an overlapping range, so coalesce.  */
	  ptr->end_pc = MAX (ptr->end_pc, end_pc);
	  return;
	}
      prev = ptr;
    }

  h = xmalloc (sizeof (struct eh_range));
  h->start_pc = start_pc;
  h->end_pc = end_pc;
  h->first_child = NULL;
  h->outer = NULL;
  h->handlers = build_tree_list (type, handler);
  h->next_sibling = NULL;
  h->expanded = 0;

  if (prev == NULL)
    whole_range.first_child = h;
  else
    prev->next_sibling = h;
}


/* if there are any handlers for this range, issue start of region */
static void
expand_start_java_handler (range)
  struct eh_range *range;
{
#if defined(DEBUG_JAVA_BINDING_LEVELS)
  indent ();
  fprintf (stderr, "expand start handler pc %d --> %d\n",
	   current_pc, range->end_pc);
#endif /* defined(DEBUG_JAVA_BINDING_LEVELS) */
  range->expanded = 1;
  expand_eh_region_start ();
}

tree
prepare_eh_table_type (type)
    tree type;
{
  tree exp;

  /* The "type" (metch_info) in a (Java) exception table is one:
   * a) NULL - meaning match any type in a try-finally.
   * b) a pointer to a (ccmpiled) class (low-order bit 0).
   * c) a pointer to the Utf8Const name of the class, plus one
   * (which yields a value with low-order bit 1). */

  if (type == NULL_TREE)
    exp = NULL_TREE;
  else if (is_compiled_class (type))
    exp = build_class_ref (type);
  else
    exp = fold (build 
		(PLUS_EXPR, ptr_type_node,
		 build_utf8_ref (build_internal_class_name (type)),
		 size_one_node));
  return exp;
}


/* Build a reference to the jthrowable object being carried in the
   exception header.  */

tree
build_exception_object_ref (type)
     tree type;
{
  tree obj;

  /* Java only passes object via pointer and doesn't require adjusting.
     The java object is immediately before the generic exception header.  */
  obj = build (EXC_PTR_EXPR, build_pointer_type (type));
  obj = build (MINUS_EXPR, TREE_TYPE (obj), obj,
	       TYPE_SIZE_UNIT (TREE_TYPE (obj)));
  obj = build1 (INDIRECT_REF, type, obj);

  return obj;
}

/* If there are any handlers for this range, isssue end of range,
   and then all handler blocks */
static void
expand_end_java_handler (range)
     struct eh_range *range;
{  
  tree handler = range->handlers;
  force_poplevels (range->start_pc);
  expand_start_all_catch ();
  for ( ; handler != NULL_TREE; handler = TREE_CHAIN (handler))
    {
      /* For bytecode we treat exceptions a little unusually.  A
	 `finally' clause looks like an ordinary exception handler for
	 Throwable.  The reason for this is that the bytecode has
	 already expanded the finally logic, and we would have to do
	 extra (and difficult) work to get this to look like a
	 gcc-style finally clause.  */
      tree type = TREE_PURPOSE (handler);
      if (type == NULL)
	type = throwable_type_node;

      expand_start_catch (type);
      expand_goto (TREE_VALUE (handler));
      expand_end_catch ();
    }
  expand_end_all_catch ();
#if defined(DEBUG_JAVA_BINDING_LEVELS)
  indent ();
  fprintf (stderr, "expand end handler pc %d <-- %d\n",
	   current_pc, range->start_pc);
#endif /* defined(DEBUG_JAVA_BINDING_LEVELS) */
}

/* Recursive helper routine for maybe_start_handlers. */

static void
check_start_handlers (range, pc)
     struct eh_range *range;
     int pc;
{
  if (range != NULL_EH_RANGE && range->start_pc == pc)
    {
      check_start_handlers (range->outer, pc);
      if (!range->expanded)
	expand_start_java_handler (range);
    }
}


static struct eh_range *current_range;

/* Emit any start-of-try-range starting at start_pc and ending after
   end_pc. */

void
maybe_start_try (start_pc, end_pc)
     int start_pc;
     int end_pc;
{
  struct eh_range *range;
  if (! doing_eh (1))
    return;

  range = find_handler (start_pc);
  while (range != NULL_EH_RANGE && range->start_pc == start_pc
	 && range->end_pc < end_pc)
    range = range->outer;
	 
  current_range = range;
  check_start_handlers (range, start_pc);
}

/* Emit any end-of-try-range ending at end_pc and starting before
   start_pc. */

void
maybe_end_try (start_pc, end_pc)
     int start_pc;
     int end_pc;
{
  if (! doing_eh (1))
    return;

  while (current_range != NULL_EH_RANGE && current_range->end_pc <= end_pc
	 && current_range->start_pc >= start_pc)
    {
      expand_end_java_handler (current_range);
      current_range = current_range->outer;
    }
}
