/* Data structure definitions for a generic GCC target.
   Copyright (C) 2001, 2002, 2004 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

/* This file contains a data structure that describes a GCC target.
   At present it is incomplete, but in future it should grow to
   contain most or all target machine and target O/S specific
   information.

   This structure has its initializer declared in target-def.h in the
   form of large macro TARGET_INITIALIZER that expands to many smaller
   macros.

   The smaller macros each initialize one component of the structure,
   and each has a default.  Each target should have a file that
   includes target.h and target-def.h, and overrides any inappropriate
   defaults by undefining the relevant macro and defining a suitable
   replacement.  That file should then contain the definition of
   "targetm" like so:

   struct gcc_target targetm = TARGET_INITIALIZER;

   Doing things this way allows us to bring together everything that
   defines a GCC target.  By supplying a default that is appropriate
   to most targets, we can easily add new items without needing to
   edit dozens of target configuration files.  It should also allow us
   to gradually reduce the amount of conditional compilation that is
   scattered throughout GCC.  */

struct gcc_target
{
  /* Functions that output assembler for the target.  */
  struct asm_out
  {
    /* Opening and closing parentheses for asm expression grouping.  */
    const char *open_paren, *close_paren;

    /* Assembler instructions for creating various kinds of integer object.  */
    const char *byte_op;
    struct asm_int_op
    {
      const char *hi;
      const char *si;
      const char *di;
      const char *ti;
    } aligned_op, unaligned_op;

    /* Try to output the assembler code for an integer object whose
       value is given by X.  SIZE is the size of the object in bytes and
       ALIGNED_P indicates whether it is aligned.  Return true if
       successful.  Only handles cases for which BYTE_OP, ALIGNED_OP
       and UNALIGNED_OP are NULL.  */
    bool (* integer) PARAMS ((rtx x, unsigned int size, int aligned_p));

    /* Output code that will globalize a label.  */
    void (* globalize_label) PARAMS ((FILE *, const char *));

    /* Emit an assembler directive to set visibility for the symbol
       associated with the tree decl.  */
    void (* visibility) PARAMS ((tree, int));

    /* Output the assembler code for entry to a function.  */
    void (* function_prologue) PARAMS ((FILE *, HOST_WIDE_INT));

    /* Output the assembler code for end of prologue.  */
    void (* function_end_prologue) PARAMS ((FILE *));

    /* Output the assembler code for start of epilogue.  */
    void (* function_begin_epilogue) PARAMS ((FILE *));

    /* Output the assembler code for function exit.  */
    void (* function_epilogue) PARAMS ((FILE *, HOST_WIDE_INT));

    /* Switch to an arbitrary section NAME with attributes as
       specified by FLAGS.  */
    void (* named_section) PARAMS ((const char *, unsigned int));

    /* Switch to the section that holds the exception table.  */
    void (* exception_section) PARAMS ((void));

    /* Switch to the section that holds the exception frames.  */
    void (* eh_frame_section) PARAMS ((void));

    /* Select and switch to a section for EXP.  It may be a DECL or a
       constant for which TREE_CST_RTL is valid.  RELOC is nonzero if
       runtime relocations must be applied; bit 1 will be set if the
       runtime relocations require non-local name resolution.  ALIGN is
       the required alignment of the data.  */
    void (* select_section) PARAMS ((tree, int, unsigned HOST_WIDE_INT));

    /* Select and switch to a section for X with MODE.  ALIGN is
       the desired alignment of the data.  */
    void (* select_rtx_section) PARAMS ((enum machine_mode, rtx,
					 unsigned HOST_WIDE_INT));

    /* Select a unique section name for DECL.  RELOC is the same as
       for SELECT_SECTION.  */
    void (* unique_section) PARAMS ((tree, int));

    /* Output a constructor for a symbol with a given priority.  */
    void (* constructor) PARAMS ((rtx, int));

    /* Output a destructor for a symbol with a given priority.  */
    void (* destructor) PARAMS ((rtx, int));

    /* Output the assembler code for a thunk function.  THUNK_DECL is the
       declaration for the thunk function itself, FUNCTION is the decl for
       the target function.  DELTA is an immediate constant offset to be
       added to THIS.  If VCALL_OFFSET is nonzero, the word at
       *(*this + vcall_offset) should be added to THIS.  */
    void (* output_mi_thunk) PARAMS ((FILE *file, tree thunk_decl,
				      HOST_WIDE_INT delta,
				      HOST_WIDE_INT vcall_offset,
				      tree function_decl));

    /* Determine whether output_mi_thunk would succeed.  */
    /* ??? Ideally, this hook would not exist, and success or failure
       would be returned from output_mi_thunk directly.  But there's
       too much undo-able setup involved in invoking output_mi_thunk.
       Could be fixed by making output_mi_thunk emit rtl instead of
       text to the output file.  */
    bool (* can_output_mi_thunk) PARAMS ((tree thunk_decl,
				          HOST_WIDE_INT delta,
				          HOST_WIDE_INT vcall_offset,
				          tree function_decl));
  } asm_out;

  /* Functions relating to instruction scheduling.  */
  struct sched
  {
    /* Given the current cost, COST, of an insn, INSN, calculate and
       return a new cost based on its relationship to DEP_INSN through
       the dependence LINK.  The default is to make no adjustment.  */
    int (* adjust_cost) PARAMS ((rtx insn, rtx link, rtx def_insn, int cost));

    /* Adjust the priority of an insn as you see fit.  Returns the new
       priority.  */
    int (* adjust_priority) PARAMS ((rtx, int));

    /* Function which returns the maximum number of insns that can be
       scheduled in the same machine cycle.  This must be constant
       over an entire compilation.  The default is 1.  */
    int (* issue_rate) PARAMS ((void));

    /* Calculate how much this insn affects how many more insns we
       can emit this cycle.  Default is they all cost the same.  */
    int (* variable_issue) PARAMS ((FILE *, int, rtx, int));

    /* Initialize machine-dependent scheduling code.  */
    void (* md_init) PARAMS ((FILE *, int, int));

    /* Finalize machine-dependent scheduling code.  */
    void (* md_finish) PARAMS ((FILE *, int));

    /* Reorder insns in a machine-dependent fashion, in two different
       places.  Default does nothing.  */
    int (* reorder)  PARAMS ((FILE *, int, rtx *, int *, int));
    int (* reorder2) PARAMS ((FILE *, int, rtx *, int *, int));

    /* The following member value is a pointer to a function returning
       nonzero if we should use DFA based scheduling.  The default is
       to use the old pipeline scheduler.  */
    int (* use_dfa_pipeline_interface) PARAMS ((void));
    /* The values of all the following members are used only for the
       DFA based scheduler: */
    /* The values of the following four members are pointers to
       functions used to simplify the automaton descriptions.
       dfa_pre_cycle_insn and dfa_post_cycle_insn give functions
       returning insns which are used to change the pipeline hazard
       recognizer state when the new simulated processor cycle
       correspondingly starts and finishes.  The function defined by
       init_dfa_pre_cycle_insn and init_dfa_post_cycle_insn are used
       to initialize the corresponding insns.  The default values of
       the memebers result in not changing the automaton state when
       the new simulated processor cycle correspondingly starts and
       finishes.  */
    void (* init_dfa_pre_cycle_insn) PARAMS ((void));
    rtx (* dfa_pre_cycle_insn) PARAMS ((void));
    void (* init_dfa_post_cycle_insn) PARAMS ((void));
    rtx (* dfa_post_cycle_insn) PARAMS ((void));
    /* The following member value is a pointer to a function returning value
       which defines how many insns in queue `ready' will we try for
       multi-pass scheduling.  if the member value is nonzero and the
       function returns positive value, the DFA based scheduler will make
       multi-pass scheduling for the first cycle.  In other words, we will
       try to choose ready insn which permits to start maximum number of
       insns on the same cycle.  */
    int (* first_cycle_multipass_dfa_lookahead) PARAMS ((void));
    /* The values of the following members are pointers to functions
       used to improve the first cycle multipass scheduling by
       inserting nop insns.  dfa_scheduler_bubble gives a function
       returning a nop insn with given index.  The indexes start with
       zero.  The function should return NULL if there are no more nop
       insns with indexes greater than given index.  To initialize the
       nop insn the function given by member
       init_dfa_scheduler_bubbles is used.  The default values of the
       members result in not inserting nop insns during the multipass
       scheduling.  */
    void (* init_dfa_bubbles) PARAMS ((void));
    rtx (* dfa_bubble) PARAMS ((int));
  } sched;

  /* Given two decls, merge their attributes and return the result.  */
  tree (* merge_decl_attributes) PARAMS ((tree, tree));

  /* Given two types, merge their attributes and return the result.  */
  tree (* merge_type_attributes) PARAMS ((tree, tree));

  /* Table of machine attributes and functions to handle them.
     Ignored if NULL.  */
  const struct attribute_spec *attribute_table;

  /* Return zero if the attributes on TYPE1 and TYPE2 are incompatible,
     one if they are compatible and two if they are nearly compatible
     (which causes a warning to be generated).  */
  int (* comp_type_attributes) PARAMS ((tree type1, tree type2));

  /* Assign default attributes to the newly defined TYPE.  */
  void (* set_default_type_attributes) PARAMS ((tree type));

  /* Insert attributes on the newly created DECL.  */
  void (* insert_attributes) PARAMS ((tree decl, tree *attributes));

  /* Return true if FNDECL (which has at least one machine attribute)
     can be inlined despite its machine attributes, false otherwise.  */
  bool (* function_attribute_inlinable_p) PARAMS ((tree fndecl));

  /* Return true if bitfields in RECORD_TYPE should follow the
     Microsoft Visual C++ bitfield layout rules.  */
  bool (* ms_bitfield_layout_p) PARAMS ((tree record_type));

  /* Set up target-specific built-in functions.  */
  void (* init_builtins) PARAMS ((void));

  /* Expand a target-specific builtin.  */
  rtx (* expand_builtin) PARAMS ((tree exp, rtx target, rtx subtarget,
				  enum machine_mode mode, int ignore));

  /* Given a decl, a section name, and whether the decl initializer
     has relocs, choose attributes for the section.  */
  /* ??? Should be merged with SELECT_SECTION and UNIQUE_SECTION.  */
  unsigned int (* section_type_flags) PARAMS ((tree, const char *, int));

  /* True if new jumps cannot be created, to replace existing ones or
     not, at the current point in the compilation.  */
  bool (* cannot_modify_jumps_p) PARAMS ((void));

  /* True if the constant X cannot be placed in the constant pool.  */
  bool (* cannot_force_const_mem) PARAMS ((rtx));

  /* True if EXP should be placed in a "small data" section.  */
  bool (* in_small_data_p) PARAMS ((tree));

  /* True if EXP names an object for which name resolution must resolve
     to the current module.  */
  bool (* binds_local_p) PARAMS ((tree));

  /* Do something target-specific to record properties of the DECL into
     the associated SYMBOL_REF.  */
  void (* encode_section_info) PARAMS ((tree, int));

  /* Undo the effects of encode_section_info on the symbol string.  */
  const char * (* strip_name_encoding) PARAMS ((const char *));

  /* Fetch the fixed register(s) which hold condition codes, for
     targets where it makes sense to look for duplicate assignments to
     the condition codes.  This should return true if there is such a
     register, false otherwise.  The arguments should be set to the
     fixed register numbers.  Up to two condition code registers are
     supported.  If there is only one for this target, the int pointed
     at by the second argument should be set to -1.  */
  bool (* fixed_condition_code_regs) PARAMS ((unsigned int *, unsigned int *));

  /* If two condition code modes are compatible, return a condition
     code mode which is compatible with both, such that a comparison
     done in the returned mode will work for both of the original
     modes.  If the condition code modes are not compatible, return
     VOIDmode.  */
  enum machine_mode (* cc_modes_compatible) PARAMS ((enum machine_mode,
						     enum machine_mode));

  /* Leave the boolean fields at the end.  */

  /* True if arbitrary sections are supported.  */
  bool have_named_sections;

  /* True if "native" constructors and destructors are supported,
     false if we're using collect2 for the job.  */
  bool have_ctors_dtors;

  /* True if thread-local storage is supported.  */
  bool have_tls;

  /* True if a small readonly data section is supported.  */
  bool have_srodata_section;

  /* True if EH frame info sections should be zero-terminated.  */
  bool terminate_dw2_eh_frame_info;
};

extern struct gcc_target targetm;
