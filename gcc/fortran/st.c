/* Build executable statement trees.
   Copyright (C) 2000-2014 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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

/* Executable statements are strung together into a singly linked list
   of code structures.  These structures are later translated into GCC
   GENERIC tree structures and from there to executable code for a
   target.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "gfortran.h"

gfc_code new_st;


/* Zeroes out the new_st structure.  */

void
gfc_clear_new_st (void)
{
  memset (&new_st, '\0', sizeof (new_st));
  new_st.op = EXEC_NOP;
}


/* Get a gfc_code structure, initialized with the current locus
   and a statement code 'op'.  */

gfc_code *
gfc_get_code (gfc_exec_op op)
{
  gfc_code *c;

  c = XCNEW (gfc_code);
  c->op = op;
  c->loc = gfc_current_locus;
  return c;
}


/* Given some part of a gfc_code structure, append a set of code to
   its tail, returning a pointer to the new tail.  */

gfc_code *
gfc_append_code (gfc_code *tail, gfc_code *new_code)
{
  if (tail != NULL)
    {
      while (tail->next != NULL)
	tail = tail->next;

      tail->next = new_code;
    }

  while (new_code->next != NULL)
    new_code = new_code->next;

  return new_code;
}


/* Free a single code structure, but not the actual structure itself.  */

void
gfc_free_statement (gfc_code *p)
{
  if (p->expr1)
    gfc_free_expr (p->expr1);
  if (p->expr2)
    gfc_free_expr (p->expr2);

  switch (p->op)
    {
    case EXEC_NOP:
    case EXEC_END_BLOCK:
    case EXEC_END_NESTED_BLOCK:
    case EXEC_ASSIGN:
    case EXEC_INIT_ASSIGN:
    case EXEC_GOTO:
    case EXEC_CYCLE:
    case EXEC_RETURN:
    case EXEC_END_PROCEDURE:
    case EXEC_IF:
    case EXEC_PAUSE:
    case EXEC_STOP:
    case EXEC_ERROR_STOP:
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
    case EXEC_CRITICAL:
    case EXEC_SYNC_ALL:
    case EXEC_SYNC_IMAGES:
    case EXEC_SYNC_MEMORY:
    case EXEC_LOCK:
    case EXEC_UNLOCK:
      break;

    case EXEC_BLOCK:
      gfc_free_namespace (p->ext.block.ns);
      gfc_free_association_list (p->ext.block.assoc);
      break;

    case EXEC_COMPCALL:
    case EXEC_CALL_PPC:
    case EXEC_CALL:
    case EXEC_ASSIGN_CALL:
      gfc_free_actual_arglist (p->ext.actual);
      break;

    case EXEC_SELECT:
    case EXEC_SELECT_TYPE:
      if (p->ext.block.case_list)
	gfc_free_case_list (p->ext.block.case_list);
      break;

    case EXEC_DO:
      gfc_free_iterator (p->ext.iterator, 1);
      break;

    case EXEC_ALLOCATE:
    case EXEC_DEALLOCATE:
      gfc_free_alloc_list (p->ext.alloc.list);
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

    case EXEC_WAIT:
      gfc_free_wait (p->ext.wait);
      break;

    case EXEC_READ:
    case EXEC_WRITE:
      gfc_free_dt (p->ext.dt);
      break;

    case EXEC_DT_END:
      /* The ext.dt member is a duplicate pointer and doesn't need to
	 be freed.  */
      break;

    case EXEC_DO_CONCURRENT:
    case EXEC_FORALL:
      gfc_free_forall_iterator (p->ext.forall_iterator);
      break;

    case EXEC_OMP_CANCEL:
    case EXEC_OMP_CANCELLATION_POINT:
    case EXEC_OMP_DO:
    case EXEC_OMP_DO_SIMD:
    case EXEC_OMP_END_SINGLE:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_DO_SIMD:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_SECTIONS:
    case EXEC_OMP_SIMD:
    case EXEC_OMP_SINGLE:
    case EXEC_OMP_TASK:
    case EXEC_OMP_WORKSHARE:
    case EXEC_OMP_PARALLEL_WORKSHARE:
      gfc_free_omp_clauses (p->ext.omp_clauses);
      break;

    case EXEC_OMP_CRITICAL:
      free (CONST_CAST (char *, p->ext.omp_name));
      break;

    case EXEC_OMP_FLUSH:
      gfc_free_omp_namelist (p->ext.omp_namelist);
      break;

    case EXEC_OMP_ATOMIC:
    case EXEC_OMP_BARRIER:
    case EXEC_OMP_MASTER:
    case EXEC_OMP_ORDERED:
    case EXEC_OMP_END_NOWAIT:
    case EXEC_OMP_TASKGROUP:
    case EXEC_OMP_TASKWAIT:
    case EXEC_OMP_TASKYIELD:
      break;

    default:
      gfc_internal_error ("gfc_free_statement(): Bad statement");
    }
}


/* Free a code statement and all other code structures linked to it.  */

void
gfc_free_statements (gfc_code *p)
{
  gfc_code *q;

  for (; p; p = q)
    {
      q = p->next;

      if (p->block)
	gfc_free_statements (p->block);
      gfc_free_statement (p);
      free (p);
    }
}


/* Free an association list (of an ASSOCIATE statement).  */

void
gfc_free_association_list (gfc_association_list* assoc)
{
  if (!assoc)
    return;

  gfc_free_association_list (assoc->next);
  free (assoc);
}
