/* Form lists of pseudo register references for autoinc optimization
   for GNU compiler.  This is part of flow optimization.
   Copyright (C) 1999, 2000, 2001, 2003 Free Software Foundation, Inc.
   Contributed by Michael P. Hayes (m.hayes@elec.canterbury.ac.nz)

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

#define DF_RD		 1	/* Reaching definitions.  */
#define DF_RU		 2	/* Reaching uses.  */
#define DF_LR		 4	/* Live registers.  */
#define DF_DU_CHAIN	 8	/* Def-use chain.  */
#define DF_UD_CHAIN     16	/* Use-def chain.  */
#define DF_REG_INFO	32	/* Register info.  */
#define DF_RD_CHAIN	64	/* Reg-def chain.  */
#define DF_RU_CHAIN    128	/* Reg-use chain.  */
#define DF_ALL	       255
#define DF_HARD_REGS  1024	/* Mark hard registers.  */
#define DF_EQUIV_NOTES 2048	/* Mark uses present in EQUIV/EQUAL notes.  */
#define DF_FOR_REGALLOC 4096    /* If called for the register allocator.  */

enum df_ref_type {DF_REF_REG_DEF, DF_REF_REG_USE, DF_REF_REG_MEM_LOAD,
		  DF_REF_REG_MEM_STORE};

#define DF_REF_TYPE_NAMES {"def", "use", "mem load", "mem store"}

/* Link on a def-use or use-def chain.  */
struct df_link
{
  struct df_link *next;
  struct ref *ref;
};

enum df_ref_flags
  {
    /* Read-modify-write refs generate both a use and a def and
       these are marked with this flag to show that they are not
       independent.  */
    DF_REF_READ_WRITE = 1,

    /* This flag is set on register references inside a subreg on
       machines which have CANNOT_CHANGE_MODE_CLASS.
       Note, that this flag can also be set on df_refs representing
       the REG itself (i.e., one might not see the subreg anyore).
       Also note, that this flag is set also for hardreg refs, i.e.,
       you must check yourself if it's a pseudo.  */
    DF_REF_MODE_CHANGE = 2,

    /* This flag is set, if we stripped the subreg from the reference.
       In this case we must make conservative guesses, at what the
       outer mode was.  */
    DF_REF_STRIPPED = 4,

    /* This flag is set during register allocation if it's okay for
    the reference's INSN to have one of its operands replaced with a
    memory reference.  */
    DF_REF_MEM_OK = 8
  };


/* Define a register reference structure.  One of these is allocated
   for every register reference (use or def).  Note some register
   references (e.g., post_inc, subreg) generate both a def and a use.  */
struct ref
{
  rtx reg;			/* The register referenced.  */
  rtx insn;			/* Insn containing ref.  */
  rtx *loc;			/* The location of the reg.  */
  struct df_link *chain;	/* Head of def-use or use-def chain.  */
  unsigned int id;		/* Ref index.  */
  enum df_ref_type type;	/* Type of ref.  */
  enum df_ref_flags flags;	/* Various flags.  */
};


/* One of these structures is allocated for every insn.  */
struct insn_info
{
  struct df_link *defs;		/* Head of insn-def chain.  */
  struct df_link *uses;		/* Head of insn-use chain.  */
  /* ???? The following luid field should be considered private so that
     we can change it on the fly to accommodate new insns?  */
  int luid;			/* Logical UID.  */
};


/* One of these structures is allocated for every reg.  */
struct reg_info
{
  struct df_link *defs;		/* Head of reg-def chain.  */
  struct df_link *uses;		/* Head of reg-use chain.  */
  int lifetime;
  int n_defs;
  int n_uses;
};


/* One of these structures is allocated for every basic block.  */
struct bb_info
{
  /* Reaching def bitmaps have def_id elements.  */
  bitmap rd_kill;
  bitmap rd_gen;
  bitmap rd_in;
  bitmap rd_out;
  /* Reaching use bitmaps have use_id elements.  */
  bitmap ru_kill;
  bitmap ru_gen;
  bitmap ru_in;
  bitmap ru_out;
  /* Live variable bitmaps have n_regs elements.  */
  bitmap lr_def;
  bitmap lr_use;
  bitmap lr_in;
  bitmap lr_out;
  int rd_valid;
  int ru_valid;
  int lr_valid;
};


struct df
{
  int flags;			/* Indicates what's recorded.  */
  struct bb_info *bbs;		/* Basic block table.  */
  struct ref **defs;		/* Def table, indexed by def_id.  */
  struct ref **uses;		/* Use table, indexed by use_id.  */
  struct ref **reg_def_last;	/* Indexed by regno.  */
  struct reg_info *regs;	/* Regs table, index by regno.  */
  unsigned int reg_size;	/* Size of regs table.  */
  struct insn_info *insns;	/* Insn table, indexed by insn UID.  */
  unsigned int insn_size;	/* Size of insn table.  */
  unsigned int def_id;		/* Next def ID.  */
  unsigned int def_size;	/* Size of def table.  */
  unsigned int n_defs;		/* Size of def bitmaps.  */
  unsigned int use_id;		/* Next use ID.  */
  unsigned int use_size;	/* Size of use table.  */
  unsigned int n_uses;		/* Size of use bitmaps.  */
  unsigned int n_bbs;		/* Number of basic blocks.  */
  unsigned int n_regs;		/* Number of regs.  */
  unsigned int def_id_save;	/* Saved next def ID.  */
  unsigned int use_id_save;	/* Saved next use ID.  */
  bitmap insns_modified;	/* Insns that (may) have changed.  */
  bitmap bbs_modified;		/* Blocks that (may) have changed.  */
  bitmap all_blocks;		/* All blocks in CFG.  */
  /* The sbitmap vector of dominators or NULL if not computed.
     Ideally, this should be a pointer to a CFG object.  */
  sbitmap *dom;
  int *dfs_order;		/* DFS order -> block number.  */
  int *rc_order;		/* Reverse completion order -> block number.  */
  int *rts_order;		/* Reverse top sort order -> block number.  */
  int *inverse_rc_map;		/* Block number -> reverse completion order.  */
  int *inverse_dfs_map;		/* Block number -> DFS order.  */
  int *inverse_rts_map;		/* Block number -> reverse top-sort order.  */
};


struct df_map
{
  rtx old;
  rtx new;
};


#define DF_BB_INFO(REFS, BB) (&REFS->bbs[(BB)->index])


/* Macros to access the elements within the ref structure.  */

#define DF_REF_REAL_REG(REF) (GET_CODE ((REF)->reg) == SUBREG \
				? SUBREG_REG ((REF)->reg) : ((REF)->reg))
#define DF_REF_REGNO(REF) REGNO (DF_REF_REAL_REG (REF))
#define DF_REF_REAL_LOC(REF) (GET_CODE ((REF)->reg) == SUBREG \
			        ? &SUBREG_REG ((REF)->reg) : ((REF)->loc))
#define DF_REF_REG(REF) ((REF)->reg)
#define DF_REF_LOC(REF) ((REF)->loc)
#define DF_REF_BB(REF) (BLOCK_FOR_INSN ((REF)->insn))
#define DF_REF_BBNO(REF) (BLOCK_FOR_INSN ((REF)->insn)->index)
#define DF_REF_INSN(REF) ((REF)->insn)
#define DF_REF_INSN_UID(REF) (INSN_UID ((REF)->insn))
#define DF_REF_TYPE(REF) ((REF)->type)
#define DF_REF_CHAIN(REF) ((REF)->chain)
#define DF_REF_ID(REF) ((REF)->id)
#define DF_REF_FLAGS(REF) ((REF)->flags)

/* Macros to determine the reference type.  */

#define DF_REF_REG_DEF_P(REF) (DF_REF_TYPE (REF) == DF_REF_REG_DEF)
#define DF_REF_REG_USE_P(REF) ((REF) && ! DF_REF_REG_DEF_P (REF))
#define DF_REF_REG_MEM_STORE_P(REF) (DF_REF_TYPE (REF) == DF_REF_REG_MEM_STORE)
#define DF_REF_REG_MEM_LOAD_P(REF) (DF_REF_TYPE (REF) == DF_REF_REG_MEM_LOAD)
#define DF_REF_REG_MEM_P(REF) (DF_REF_REG_MEM_STORE_P (REF) \
                               || DF_REF_REG_MEM_LOAD_P (REF))


/* Macros to access the elements within the reg_info structure table.  */

#define DF_REGNO_FIRST_DEF(DF, REGNUM) \
((DF)->regs[REGNUM].defs ? (DF)->regs[REGNUM].defs->ref : 0)
#define DF_REGNO_LAST_USE(DF, REGNUM) \
((DF)->regs[REGNUM].uses ? (DF)->regs[REGNUM].uses->ref : 0)

#define DF_REGNO_FIRST_BB(DF, REGNUM) \
(DF_REGNO_FIRST_DEF (DF, REGNUM) \
? DF_REF_BB (DF_REGNO_FIRST_DEF (DF, REGNUM)) : 0)
#define DF_REGNO_LAST_BB(DF, REGNUM) \
(DF_REGNO_LAST_USE (DF, REGNUM) \
? DF_REF_BB (DF_REGNO_LAST_USE (DF, REGNUM)) : 0)


/* Macros to access the elements within the insn_info structure table.  */

#define DF_INSN_LUID(DF, INSN) ((DF)->insns[INSN_UID (INSN)].luid)
#define DF_INSN_DEFS(DF, INSN) ((DF)->insns[INSN_UID (INSN)].defs)
#define DF_INSN_USES(DF, INSN) ((DF)->insns[INSN_UID (INSN)].uses)


/* Functions to build and analyse dataflow information.  */

extern struct df *df_init (void);

extern int df_analyse (struct df *, bitmap, int);

extern void df_finish (struct df *);

extern void df_dump (struct df *, int, FILE *);


/* Functions to modify insns.  */

extern void df_insn_modify (struct df *, basic_block, rtx);

extern rtx df_insn_delete (struct df *, basic_block, rtx);

extern rtx df_pattern_emit_before (struct df *, rtx, basic_block, rtx);

extern rtx df_jump_pattern_emit_after (struct df *, rtx, basic_block, rtx);

extern rtx df_pattern_emit_after (struct df *, rtx, basic_block, rtx);

extern rtx df_insn_move_before (struct df *, basic_block, rtx, basic_block,
				rtx);

extern int df_reg_replace (struct df *, bitmap, rtx, rtx);

extern int df_ref_reg_replace (struct df *, struct ref *, rtx, rtx);

extern int df_ref_remove (struct df *, struct ref *);

extern int df_insn_reg_replace (struct df *, basic_block, rtx, rtx, rtx);

extern int df_insn_mem_replace (struct df *, basic_block, rtx, rtx, rtx);

extern struct ref *df_bb_def_use_swap (struct df *, basic_block, rtx, rtx,
				       unsigned int);


/* Functions to query dataflow information.  */

extern basic_block df_regno_bb (struct df *, unsigned int);

extern int df_reg_lifetime (struct df *, rtx);

extern int df_reg_global_p (struct df *, rtx);

extern int df_insn_regno_def_p (struct df *, basic_block, rtx, unsigned int);

extern int df_insn_dominates_all_uses_p (struct df *, basic_block, rtx);

extern int df_insn_dominates_uses_p (struct df *, basic_block, rtx, bitmap);

extern int df_bb_reg_live_start_p (struct df *, basic_block, rtx);

extern int df_bb_reg_live_end_p (struct df *, basic_block, rtx);

extern int df_bb_regs_lives_compare (struct df *, basic_block, rtx, rtx);

extern rtx df_bb_single_def_use_insn_find (struct df *, basic_block, rtx,
					   rtx);


/* Functions for debugging from GDB.  */

extern void debug_df_insn (rtx);

extern void debug_df_regno (unsigned int);

extern void debug_df_reg (rtx);

extern void debug_df_defno (unsigned int);

extern void debug_df_useno (unsigned int);

extern void debug_df_ref (struct ref *);

extern void debug_df_chain (struct df_link *);

extern void df_insn_debug (struct df *, rtx, FILE *);

extern void df_insn_debug_regno (struct df *, rtx, FILE *);


/* Meet over any path (UNION) or meet over all paths (INTERSECTION).  */
enum df_confluence_op
  {
    DF_UNION,
    DF_INTERSECTION
  };


/* Dataflow direction.  */
enum df_flow_dir
  {
    DF_FORWARD,
    DF_BACKWARD
  };


typedef void (*transfer_function_sbitmap) (int, int *, sbitmap, sbitmap,
					   sbitmap, sbitmap, void *);

typedef void (*transfer_function_bitmap) (int, int *, bitmap, bitmap,
					  bitmap, bitmap, void *);

extern void iterative_dataflow_sbitmap (sbitmap *, sbitmap *, sbitmap *,
					sbitmap *, bitmap, enum df_flow_dir,
					enum df_confluence_op,
					transfer_function_sbitmap,
					int *, void *);

extern void iterative_dataflow_bitmap (bitmap *, bitmap *, bitmap *,
				       bitmap *, bitmap,
				       enum df_flow_dir,
				       enum df_confluence_op,
				       transfer_function_bitmap,
				       int *, void *);
extern bool read_modify_subreg_p (rtx);
