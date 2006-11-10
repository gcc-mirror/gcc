/* Build executable statement trees.
   Copyright (C) 2000, 2001, 2002, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* Executable statements are strung together into a singly linked list
   of code structures.  These structures are later translated into GCC
   GENERIC tree structures and from there to executable code for a
   target.  */

#include "config.h"
#include "system.h"
#include "gfortran.h"

gfc_code new_st;


/* Zeroes out the new_st structure.  */

void
gfc_clear_new_st (void)
{

  memset (&new_st, '\0', sizeof (new_st));
  new_st.op = EXEC_NOP;
}


/* Get a gfc_code structure.  */

gfc_code *
gfc_get_code (void)
{
  gfc_code *c;

  c = gfc_getmem (sizeof (gfc_code));
  c->loc = gfc_current_locus;
  return c;
}


/* Given some part of a gfc_code structure, append a set of code to
   its tail, returning a pointer to the new tail.  */

gfc_code *
gfc_append_code (gfc_code * tail, gfc_code * new)
{

  if (tail != NULL)
    {
      while (tail->next != NULL)
	tail = tail->next;

      tail->next = new;
    }

  while (new->next != NULL)
    new = new->next;

  return new;
}


/* Free a single code structure, but not the actual structure itself.  */

void
gfc_free_statement (gfc_code * p)
{

  if (p->expr)
    gfc_free_expr (p->expr);
  if (p->expr2)
    gfc_free_expr (p->expr2);

  switch (p->op)
    {
    case EXEC_NOP:
    case EXEC_ASSIGN:
    case EXEC_INIT_ASSIGN:
    case EXEC_GOTO:
    case EXEC_CYCLE:
    case EXEC_RETURN:
    case EXEC_IF:
    case EXEC_PAUSE:
    case EXEC_STOP:
    case EXEC_EXIT:
    case EXEC_WHERE:
    case EXEC_IOLENGTH:
    case EXEC_POINTER_ASSIGN:
    case EXEC_DO_WHILE:
    case EXEC_CONTINUE:
    case EXEC_TRANSFER:
    case EXEC_LABEL_ASSIGN:
    case EXEC_ENTRY:
    case EXEC_ARITHMETIC_IF:
      break;

    case EXEC_CALL:
    case EXEC_ASSIGN_CALL:
      gfc_free_actual_arglist (p->ext.actual);
      break;

    case EXEC_SELECT:
      if (p->ext.case_list)
	gfc_free_case_list (p->ext.case_list);
      break;

    case EXEC_DO:
      gfc_free_iterator (p->ext.iterator, 1);
      break;

    case EXEC_ALLOCATE:
    case EXEC_DEALLOCATE:
      gfc_free_alloc_list (p->ext.alloc_list);
      break;

    case EXEC_OPEN:
      gfc_free_open (p->ext.open);
      break;

    case EXEC_CLOSE:
      gfc_free_close (p->ext.close);
      break;

    case EXEC_BACKSPACE:
    case EXEC_ENDFILE:
    case EXEC_REWIND:
    case EXEC_FLUSH:
      gfc_free_filepos (p->ext.filepos);
      break;

    case EXEC_INQUIRE:
      gfc_free_inquire (p->ext.inquire);
      break;

    case EXEC_READ:
    case EXEC_WRITE:
      gfc_free_dt (p->ext.dt);
      break;

    case EXEC_DT_END:
      /* The ext.dt member is a duplicate pointer and doesn't need to
         be freed.  */
      break;

    case EXEC_FORALL:
      gfc_free_forall_iterator (p->ext.forall_iterator);
      break;

    default:
      gfc_internal_error ("gfc_free_statement(): Bad statement");
    }
}


/* Free a code statement and all other code structures linked to it.  */

void
gfc_free_statements (gfc_code * p)
{
  gfc_code *q;

  for (; p; p = q)
    {
      q = p->next;

      if (p->block)
	gfc_free_statements (p->block);
      gfc_free_statement (p);
      gfc_free (p);
    }
}

