/* Handle exceptions for GNU compiler for the Java(TM) language.
   Copyright (C) 1997 Free Software Foundation, Inc.

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
#include "except.h"
#include "java-except.h"
#include "eh-common.h"
#include "toplev.h"

extern struct obstack permanent_obstack;

struct eh_range *current_method_handlers;

struct eh_range *current_try_block = NULL;

struct eh_range *eh_range_freelist = NULL;

/* These variables are used to speed up find_handler. */

static int cache_range_start, cache_range_end;
static struct eh_range *cache_range;
static struct eh_range *cache_next_child;

/* A dummy range that represents the entire method. */

struct eh_range whole_range;

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
      if (pc <= child->end_pc)
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

#if 0
first_child;
next_sibling;
outer;
#endif

/* Recursive helper routine for add_handler. */

static int
link_handler (start_pc, end_pc, handler, type, outer)
     int start_pc, end_pc;
     tree handler;
     tree type;
     struct eh_range *outer;
{
  struct eh_range **ptr;
  if (start_pc < outer->start_pc || end_pc > outer->end_pc)
    return 0;  /* invalid or non-nested exception range */
  if (start_pc == outer->start_pc && end_pc == outer->end_pc)
    {
      outer->handlers = tree_cons (type, handler, outer->handlers);
      return 1;
    }
  ptr = &outer->first_child;
  for (;; ptr = &(*ptr)->next_sibling)
    {
      if (*ptr == NULL || end_pc <= (*ptr)->start_pc)
	{
	  struct eh_range *h = (struct eh_range *)
	    oballoc (sizeof (struct eh_range));
	  h->start_pc = start_pc;
	  h->end_pc = end_pc;
	  h->next_sibling = *ptr;
	  h->first_child = NULL;
	  h->outer = outer;
	  h->handlers = build_tree_list (type, handler);
	  *ptr = h;
	  return 1;
	}
      else if (start_pc < (*ptr)->end_pc)
	return link_handler (start_pc, end_pc, handler, type, *ptr);
      /* end_pc > (*ptr)->start_pc && start_pc >= (*ptr)->end_pc. */
    }
}

/* Called to re-initialize the exception machinery for a new method. */

void
method_init_exceptions ()
{
  whole_range.start_pc = 0;
  whole_range.end_pc = DECL_CODE_LENGTH (current_function_decl) + 1;
  whole_range.outer = NULL;
  whole_range.first_child = NULL;
  whole_range.next_sibling = NULL;
  cache_range_start = 0xFFFFFF;
  java_set_exception_lang_code ();
}

void
java_set_exception_lang_code ()
{
  set_exception_lang_code (EH_LANG_Java);
  set_exception_version_code (1);
}

int
add_handler (start_pc, end_pc, handler, type)
     int start_pc, end_pc;
     tree handler;
     tree type;
{
  return link_handler (start_pc, end_pc, handler, type, &whole_range);
}


/* if there are any handlers for this range, issue start of region */
void
expand_start_java_handler (range)
     struct eh_range *range;
{
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

  push_obstacks (&permanent_obstack, &permanent_obstack);
  if (type == NULL_TREE)
    exp = null_pointer_node;
  else if (is_compiled_class (type))
    exp = build_class_ref (type);
  else
    exp = fold (build 
		(PLUS_EXPR, ptr_type_node,
		 build_utf8_ref (build_internal_class_name (type)),
		 size_one_node));
  pop_obstacks ();
  return exp;
}

/* if there are any handlers for this range, isssue end of range,
   and then all handler blocks */
void
expand_end_java_handler (range)
     struct eh_range *range;
{
  tree handler = range->handlers;
  expand_start_all_catch ();
  for ( ; handler != NULL_TREE; handler = TREE_CHAIN (handler))
    {
      start_catch_handler (prepare_eh_table_type (TREE_PURPOSE (handler)));
      /* Push the thrown object on the top of the stack */
      expand_goto (TREE_VALUE (handler));
    }
  expand_end_all_catch ();
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
      expand_start_java_handler (range);
    }
}

struct eh_range *current_range;

/* Emit any start-of-try-range start at PC. */

void
maybe_start_try (pc)
     int pc;
{
  if (! doing_eh (1))
    return;

  current_range = find_handler (pc);
  check_start_handlers (current_range, pc);
}

/* Emit any end-of-try-range end at PC. */

void
maybe_end_try (pc)
     int pc;
{
  if (! doing_eh (1))
    return;

  while (current_range != NULL_EH_RANGE && current_range->end_pc <= pc)
    {
      expand_end_java_handler (current_range);
      current_range = current_range->outer;
    }
}

/* Emit the handler labels and their code */

void
emit_handlers ()
{
  if (catch_clauses)
    {
      rtx funcend = gen_label_rtx ();
      emit_jump (funcend);

      emit_insns (catch_clauses);
      expand_leftover_cleanups ();

      emit_label (funcend);
    }
}
