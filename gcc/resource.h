/* Definitions for computing resource usage of specific insns.
   Copyright (C) 1999 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.  */

/* Macro to clear all resources.  */
#define CLEAR_RESOURCE(RES)	\
 do { (RES)->memory = (RES)->unch_memory = (RES)->volatil = (RES)->cc = 0; \
      CLEAR_HARD_REG_SET ((RES)->regs); } while (0)

/* The resources used by a given insn. */
struct resources
{
  char memory;		/* Insn sets or needs a memory location.  */
  char unch_memory;	/* Insn sets of needs a "unchanging" MEM.  */
  char volatil;		/* Insn sets or needs a volatile memory loc.  */
  char cc;		/* Insn sets or needs the condition codes.  */
  HARD_REG_SET regs;	/* Which registers are set or needed.  */
};

extern void mark_target_live_regs 	PROTO((rtx, rtx, struct resources *));
extern void mark_set_resources		PROTO((rtx, struct resources *, int,
					       int));
extern void mark_referenced_resources	PROTO((rtx, struct resources *, int));
extern void clear_hashed_info_for_insn	PROTO((rtx));
extern void incr_ticks_for_insn		PROTO((rtx));
extern void mark_end_of_function_resources PROTO ((rtx, int));
extern void init_resource_info		PROTO((rtx));
extern void free_resource_info		PROTO((void));
extern rtx find_free_register		PROTO((rtx, char *, int,
					       HARD_REG_SET *));
