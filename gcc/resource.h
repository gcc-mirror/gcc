/* Definitions for computing resource usage of specific insns.
   Copyright (C) 1999 Free Software Foundation, Inc.

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Macro to clear all resources.  */
#define CLEAR_RESOURCE(RES)	\
 do { (RES)->memory = (RES)->unch_memory = (RES)->volatil = (RES)->cc = 0; \
      CLEAR_HARD_REG_SET ((RES)->regs); } while (0)

/* The resources used by a given insn.  */
struct resources
{
  char memory;		/* Insn sets or needs a memory location.  */
  char unch_memory;	/* Insn sets of needs a "unchanging" MEM.  */
  char volatil;		/* Insn sets or needs a volatile memory loc.  */
  char cc;		/* Insn sets or needs the condition codes.  */
  HARD_REG_SET regs;	/* Which registers are set or needed.  */
};

/* The kinds of rtl mark_*_resources will consider */
enum mark_resource_type
{
  MARK_SRC_DEST = 0,
  MARK_SRC_DEST_CALL = 1
};

extern void mark_target_live_regs 	PARAMS ((rtx, rtx, struct resources *));
extern void mark_set_resources		PARAMS ((rtx, struct resources *, int,
					       enum mark_resource_type));
extern void mark_referenced_resources	PARAMS ((rtx, struct resources *, int));
extern void clear_hashed_info_for_insn	PARAMS ((rtx));
extern void incr_ticks_for_insn		PARAMS ((rtx));
extern void mark_end_of_function_resources PARAMS ((rtx, int));
extern void init_resource_info		PARAMS ((rtx));
extern void free_resource_info		PARAMS ((void));
