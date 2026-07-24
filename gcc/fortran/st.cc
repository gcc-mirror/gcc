/* Build executable statement trees.
   Copyright (C) 2000-2026 Free Software Foundation, Inc.
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
  if (p->expr3)
    gfc_free_expr (p->expr3);
  if (p->expr4)
    gfc_free_expr (p->expr4);

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
    case EXEC_EVENT_POST:
    case EXEC_EVENT_WAIT:
    case EXEC_FAIL_IMAGE:
    case EXEC_CHANGE_TEAM:
    case EXEC_END_TEAM:
    case EXEC_FORM_TEAM:
    case EXEC_SYNC_TEAM:
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
    case EXEC_SELECT_RANK:
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
      for (int i = 0; i < LOCALITY_NUM; i++)
	gfc_free_expr_list (p->ext.concur.locality[i]);
      gcc_fallthrough ();
    case EXEC_FORALL:
      gfc_free_forall_iterator (p->ext.concur.forall_iterator);
      break;

    case EXEC_OACC_DECLARE:
      if (p->ext.oacc_declare)
	gfc_free_oacc_declare_clauses (p->ext.oacc_declare);
      break;

    case EXEC_OACC_ATOMIC:
    case EXEC_OACC_PARALLEL_LOOP:
    case EXEC_OACC_PARALLEL:
    case EXEC_OACC_KERNELS_LOOP:
    case EXEC_OACC_KERNELS:
    case EXEC_OACC_SERIAL_LOOP:
    case EXEC_OACC_SERIAL:
    case EXEC_OACC_DATA:
    case EXEC_OACC_HOST_DATA:
    case EXEC_OACC_LOOP:
    case EXEC_OACC_UPDATE:
    case EXEC_OACC_WAIT:
    case EXEC_OACC_CACHE:
    case EXEC_OACC_ENTER_DATA:
    case EXEC_OACC_EXIT_DATA:
    case EXEC_OACC_ROUTINE:
    case EXEC_OACC_INIT:
    case EXEC_OACC_SHUTDOWN:
    case EXEC_OACC_SET:
    case EXEC_OMP_ALLOCATE:
    case EXEC_OMP_ALLOCATORS:
    case EXEC_OMP_ASSUME:
    case EXEC_OMP_ATOMIC:
    case EXEC_OMP_CANCEL:
    case EXEC_OMP_CANCELLATION_POINT:
    case EXEC_OMP_CRITICAL:
    case EXEC_OMP_DEPOBJ:
    case EXEC_OMP_DISPATCH:
    case EXEC_OMP_DISTRIBUTE:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_DISTRIBUTE_SIMD:
    case EXEC_OMP_DO:
    case EXEC_OMP_DO_SIMD:
    case EXEC_OMP_ERROR:
    case EXEC_OMP_INTEROP:
    case EXEC_OMP_LOOP:
    case EXEC_OMP_END_SINGLE:
    case EXEC_OMP_MASKED_TASKLOOP:
    case EXEC_OMP_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_MASTER_TASKLOOP:
    case EXEC_OMP_MASTER_TASKLOOP_SIMD:
    case EXEC_OMP_ORDERED:
    case EXEC_OMP_MASKED:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_DO_SIMD:
    case EXEC_OMP_PARALLEL_LOOP:
    case EXEC_OMP_PARALLEL_MASKED:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_PARALLEL_MASTER:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_PARALLEL_WORKSHARE:
    case EXEC_OMP_SCAN:
    case EXEC_OMP_SCOPE:
    case EXEC_OMP_SECTIONS:
    case EXEC_OMP_SIMD:
    case EXEC_OMP_SINGLE:
    case EXEC_OMP_TARGET:
    case EXEC_OMP_TARGET_DATA:
    case EXEC_OMP_TARGET_ENTER_DATA:
    case EXEC_OMP_TARGET_EXIT_DATA:
    case EXEC_OMP_TARGET_PARALLEL:
    case EXEC_OMP_TARGET_PARALLEL_DO:
    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_PARALLEL_LOOP:
    case EXEC_OMP_TARGET_SIMD:
    case EXEC_OMP_TARGET_TEAMS:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TARGET_TEAMS_LOOP:
    case EXEC_OMP_TARGET_UPDATE:
    case EXEC_OMP_TASK:
    case EXEC_OMP_TASKLOOP:
    case EXEC_OMP_TASKLOOP_SIMD:
    case EXEC_OMP_TEAMS:
    case EXEC_OMP_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TEAMS_LOOP:
    case EXEC_OMP_TILE:
    case EXEC_OMP_UNROLL:
    case EXEC_OMP_WORKSHARE:
      gfc_free_omp_clauses (p->ext.omp_clauses);
      break;

    case EXEC_OMP_END_CRITICAL:
      free (const_cast<char *> (p->ext.omp_name));
      break;

    case EXEC_OMP_FLUSH:
      gfc_free_omp_namelist (p->ext.omp_namelist, OMP_LIST_NONE);
      break;

    case EXEC_OMP_BARRIER:
    case EXEC_OMP_MASTER:
    case EXEC_OMP_END_NOWAIT:
    case EXEC_OMP_TASKGROUP:
    case EXEC_OMP_TASKWAIT:
    case EXEC_OMP_TASKYIELD:
      break;

    case EXEC_OMP_METADIRECTIVE:
      gfc_free_omp_variants (p->ext.omp_variants);
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

  if (assoc->ar)
    {
      for (int i = 0; i < assoc->ar->dimen; i++)
	{
	  if (assoc->ar->start[i]
	      && assoc->ar->start[i]->ts.type == BT_INTEGER)
	    gfc_free_expr (assoc->ar->start[i]);
	  if (assoc->ar->end[i]
	      && assoc->ar->end[i]->ts.type == BT_INTEGER)
	    gfc_free_expr (assoc->ar->end[i]);
	  if (assoc->ar->stride[i]
	      && assoc->ar->stride[i]->ts.type == BT_INTEGER)
	    gfc_free_expr (assoc->ar->stride[i]);
	}
    }

  gfc_free_association_list (assoc->next);
  free (assoc);
}


/* Function to generate IF (ALLOCATED(expr)) DEALLOCATE(expr)  */

static gfc_code *
get_guarded_dealloc (gfc_namespace *ns, gfc_expr *expr)
{
  gfc_code *dealloc = gfc_get_code (EXEC_IF);
  dealloc->block = gfc_get_code (EXEC_IF);
#define ALLOCATED dealloc->block->expr1
  ALLOCATED = gfc_get_expr ();
  ALLOCATED->expr_type = EXPR_FUNCTION;
  ALLOCATED->where = gfc_current_locus;
  gfc_find_sym_tree ("allocated", ns, 1, &ALLOCATED->symtree);
  if (!ALLOCATED->symtree)
    {
      gfc_get_sym_tree ("allocated", ns, &ALLOCATED->symtree, false);
      gfc_commit_symbol (ALLOCATED->symtree->n.sym);
    }
  ALLOCATED->symtree->n.sym->attr.flavor = FL_PROCEDURE;
  ALLOCATED->symtree->n.sym->attr.intrinsic = 1;
  ALLOCATED->symtree->n.sym->result = ALLOCATED->symtree->n.sym;
  ALLOCATED->ts.type = BT_LOGICAL;
  ALLOCATED->ts.kind = gfc_default_logical_kind;
  ALLOCATED->value.function.isym
			= gfc_intrinsic_function_by_id (GFC_ISYM_ALLOCATED);
  ALLOCATED->value.function.actual = gfc_get_actual_arglist ();
  ALLOCATED->value.function.actual->expr = gfc_copy_expr (expr);
#undef ALLOCATED
  dealloc->block->next = gfc_get_code (EXEC_DEALLOCATE);
  dealloc->block->next->ext.alloc.list = gfc_get_alloc ();
  dealloc->block->next->ext.alloc.list->expr = gfc_copy_expr (expr);
  return dealloc;
}


/* F2018(11.1.5.2): Insert code to deallocate coarrays, allocated within a team
   block. This uses the previous function to effect a guarded deallocation of
   allocated coarray expressions. These are gathered in gfc_match_allocate and
   stashed in team_allocs.  */

void
deallocate_allocated_coarrays (vec<gfc_expr *> *team_allocs)
{
  gfc_code *dealloc, *last_stmt;
  gfc_ref *ref, *aref = NULL;
  int i;

  for (gfc_expr *e : *team_allocs)
    {
      if (!e)
	continue;

      /* Get the last array_ref right.  */
      for (ref = e->ref; ref; ref = ref->next)
	if (ref->type == REF_ARRAY)
	  aref = ref;

      if (aref->u.ar.as->rank)
	{
	  aref->u.ar.type = AR_FULL;
	  aref->u.ar.dimen = aref->u.ar.as->rank;
	  for (i = 0; i < aref->u.ar.dimen; i++)
	    {
	      aref->u.ar.dimen_type[i] = DIMEN_RANGE;

	      if (aref->u.ar.start[i]) gfc_free_expr (aref->u.ar.start[i]);
	      if (aref->u.ar.end[i]) gfc_free_expr (aref->u.ar.end[i]);
	      if (aref->u.ar.stride[i]) gfc_free_expr (aref->u.ar.stride[i]);
	      aref->u.ar.start[i] = aref->u.ar.end[i] = aref->u.ar.stride[i] = NULL;
	    }
	}

      for (i = aref->u.ar.as->rank;
	   i < aref->u.ar.as->rank + aref->u.ar.as->corank; i++)
	 aref->u.ar.dimen_type[i] = DIMEN_THIS_IMAGE;

      /* Insert the deallocation code before the END TEAM statement.  */
      last_stmt = gfc_current_ns->code;
      while (last_stmt)
	{
	  last_stmt = last_stmt->next;
	  if (last_stmt->next->op == EXEC_END_TEAM || !last_stmt->next)
	    {
	      dealloc = get_guarded_dealloc (gfc_current_ns, e);
	      if (dealloc)
		{
		  dealloc->next = last_stmt->next;
		  last_stmt->next = dealloc;
		  break;
		}
	    }
	}
      gfc_free_expr (e);
      e = NULL;
    }
}
