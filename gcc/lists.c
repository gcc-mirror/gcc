/* List management for the GCC expander.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "toplev.h"
#include "rtl.h"
#include "ggc.h"

static void free_list (rtx *, rtx *);

/* Functions for maintaining cache-able lists of EXPR_LIST and INSN_LISTs.  */

/* An INSN_LIST containing all INSN_LISTs allocated but currently unused.  */
static GTY ((deletable)) rtx unused_insn_list;

/* An EXPR_LIST containing all EXPR_LISTs allocated but currently unused.  */
static GTY ((deletable)) rtx unused_expr_list;

/* This function will free an entire list of either EXPR_LIST, INSN_LIST
   or DEPS_LIST nodes.  This is to be used only on lists that consist
   exclusively of nodes of one type only.  This is only called by
   free_EXPR_LIST_list, free_INSN_LIST_list and free_DEPS_LIST_list.  */
static void
free_list (rtx *listp, rtx *unused_listp)
{
  rtx link, prev_link;

  prev_link = *listp;
  link = XEXP (prev_link, 1);

  gcc_assert (unused_listp != &unused_insn_list
	      || GET_CODE (prev_link) == INSN_LIST);
  
  while (link)
    {
      gcc_assert (unused_listp != &unused_insn_list
		  || GET_CODE (prev_link) == INSN_LIST);
  
      prev_link = link;
      link = XEXP (link, 1);
    }

  XEXP (prev_link, 1) = *unused_listp;
  *unused_listp = *listp;
  *listp = 0;
}

/* Find corresponding to ELEM node in the list pointed to by LISTP.
   This node must exist in the list.  Returns pointer to that node.  */
static rtx *
find_list_elem (rtx elem, rtx *listp)
{
  while (XEXP (*listp, 0) != elem)
    listp = &XEXP (*listp, 1);
  return listp;
}

/* Remove the node pointed to by LISTP from the list.  */
static void
remove_list_node (rtx *listp)
{
  rtx node;

  node = *listp;
  *listp = XEXP (node, 1);
  XEXP (node, 1) = 0;
}

/* Removes corresponding to ELEM node from the list pointed to by LISTP.
   Returns that node.  */
rtx
remove_list_elem (rtx elem, rtx *listp)
{
  rtx node;

  listp = find_list_elem (elem, listp);
  node = *listp;
  remove_list_node (listp);
  return node;
}

/* This call is used in place of a gen_rtx_INSN_LIST. If there is a cached
   node available, we'll use it, otherwise a call to gen_rtx_INSN_LIST
   is made.  */
rtx
alloc_INSN_LIST (rtx val, rtx next)
{
  rtx r;

  if (unused_insn_list)
    {
      r = unused_insn_list;
      unused_insn_list = XEXP (r, 1);
      XEXP (r, 0) = val;
      XEXP (r, 1) = next;
      PUT_REG_NOTE_KIND (r, VOIDmode);

      gcc_assert (GET_CODE (r) == INSN_LIST);
    }
  else
    r = gen_rtx_INSN_LIST (VOIDmode, val, next);

  return r;
}

/* This call is used in place of a gen_rtx_EXPR_LIST. If there is a cached
   node available, we'll use it, otherwise a call to gen_rtx_EXPR_LIST
   is made.  */
rtx
alloc_EXPR_LIST (int kind, rtx val, rtx next)
{
  rtx r;

  if (unused_expr_list)
    {
      r = unused_expr_list;
      unused_expr_list = XEXP (r, 1);
      XEXP (r, 0) = val;
      XEXP (r, 1) = next;
      PUT_REG_NOTE_KIND (r, kind);
    }
  else
    r = gen_rtx_EXPR_LIST (kind, val, next);

  return r;
}

/* This function will free up an entire list of EXPR_LIST nodes.  */
void
free_EXPR_LIST_list (rtx *listp)
{
  if (*listp == 0)
    return;
  free_list (listp, &unused_expr_list);
}

/* This function will free up an entire list of INSN_LIST nodes.  */
void
free_INSN_LIST_list (rtx *listp)
{
  if (*listp == 0)
    return;
  free_list (listp, &unused_insn_list);
}

/* This function will free up an individual EXPR_LIST node.  */
void
free_EXPR_LIST_node (rtx ptr)
{
  XEXP (ptr, 1) = unused_expr_list;
  unused_expr_list = ptr;
}

/* This function will free up an individual INSN_LIST node.  */
void
free_INSN_LIST_node (rtx ptr)
{
  gcc_assert (GET_CODE (ptr) == INSN_LIST);
  XEXP (ptr, 1) = unused_insn_list;
  unused_insn_list = ptr;
}

/* Remove and free corresponding to ELEM node in the INSN_LIST pointed to
   by LISTP.  */
void
remove_free_INSN_LIST_elem (rtx elem, rtx *listp)
{
  free_INSN_LIST_node (remove_list_elem (elem, listp));
}

#include "gt-lists.h"
