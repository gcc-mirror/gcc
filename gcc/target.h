/* Data structure definitions for a generic GCC target.
   Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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

#include "tm.h"

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
    bool (* integer) (rtx x, unsigned int size, int aligned_p);

    /* Output code that will globalize a label.  */
    void (* globalize_label) (FILE *, const char *);

    /* Output an internal label.  */
    void (* internal_label) (FILE *, const char *, unsigned long);

    /* Emit an assembler directive to set visibility for the symbol
       associated with the tree decl.  */
    void (* visibility) (tree, int);

    /* Output the assembler code for entry to a function.  */
    void (* function_prologue) (FILE *, HOST_WIDE_INT);

    /* Output the assembler code for end of prologue.  */
    void (* function_end_prologue) (FILE *);

    /* Output the assembler code for start of epilogue.  */
    void (* function_begin_epilogue) (FILE *);

    /* Output the assembler code for function exit.  */
    void (* function_epilogue) (FILE *, HOST_WIDE_INT);

    /* Switch to an arbitrary section NAME with attributes as
       specified by FLAGS.  */
    void (* named_section) (const char *, unsigned int);

    /* Switch to the section that holds the exception table.  */
    void (* exception_section) (void);

    /* Switch to the section that holds the exception frames.  */
    void (* eh_frame_section) (void);

    /* Select and switch to a section for EXP.  It may be a DECL or a
       constant.  RELOC is nonzero if runtime relocations must be applied;
       bit 1 will be set if the runtime relocations require non-local
       name resolution.  ALIGN is the required alignment of the data.  */
    void (* select_section) (tree, int, unsigned HOST_WIDE_INT);

    /* Select and switch to a section for X with MODE.  ALIGN is
       the desired alignment of the data.  */
    void (* select_rtx_section) (enum machine_mode, rtx,
				 unsigned HOST_WIDE_INT);

    /* Select a unique section name for DECL.  RELOC is the same as
       for SELECT_SECTION.  */
    void (* unique_section) (tree, int);

    /* Output a constructor for a symbol with a given priority.  */
    void (* constructor) (rtx, int);

    /* Output a destructor for a symbol with a given priority.  */
    void (* destructor) (rtx, int);

    /* Output the assembler code for a thunk function.  THUNK_DECL is the
       declaration for the thunk function itself, FUNCTION is the decl for
       the target function.  DELTA is an immediate constant offset to be
       added to THIS.  If VCALL_OFFSET is nonzero, the word at
       *(*this + vcall_offset) should be added to THIS.  */
    void (* output_mi_thunk) (FILE *file, tree thunk_decl,
			      HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
			      tree function_decl);

    /* Determine whether output_mi_thunk would succeed.  */
    /* ??? Ideally, this hook would not exist, and success or failure
       would be returned from output_mi_thunk directly.  But there's
       too much undo-able setup involved in invoking output_mi_thunk.
       Could be fixed by making output_mi_thunk emit rtl instead of
       text to the output file.  */
    bool (* can_output_mi_thunk) (tree thunk_decl, HOST_WIDE_INT delta,
				  HOST_WIDE_INT vcall_offset,
				  tree function_decl);

    /* Output any boilerplate text needed at the beginning of a
       translation unit.  */
    void (*file_start) (void);

    /* Output any boilerplate text needed at the end of a
       translation unit.  */
    void (*file_end) (void);

    /* Output an assembler pseudo-op to declare a library function name
       external.  */
    void (*external_libcall) (rtx);
  } asm_out;

  /* Functions relating to instruction scheduling.  */
  struct sched
  {
    /* Given the current cost, COST, of an insn, INSN, calculate and
       return a new cost based on its relationship to DEP_INSN through
       the dependence LINK.  The default is to make no adjustment.  */
    int (* adjust_cost) (rtx insn, rtx link, rtx def_insn, int cost);

    /* Adjust the priority of an insn as you see fit.  Returns the new
       priority.  */
    int (* adjust_priority) (rtx, int);

    /* Function which returns the maximum number of insns that can be
       scheduled in the same machine cycle.  This must be constant
       over an entire compilation.  The default is 1.  */
    int (* issue_rate) (void);

    /* Calculate how much this insn affects how many more insns we
       can emit this cycle.  Default is they all cost the same.  */
    int (* variable_issue) (FILE *, int, rtx, int);

    /* Initialize machine-dependent scheduling code.  */
    void (* md_init) (FILE *, int, int);

    /* Finalize machine-dependent scheduling code.  */
    void (* md_finish) (FILE *, int);

    /* Reorder insns in a machine-dependent fashion, in two different
       places.  Default does nothing.  */
    int (* reorder) (FILE *, int, rtx *, int *, int);
    int (* reorder2) (FILE *, int, rtx *, int *, int);

    /* The following member value is a pointer to a function called
       after evaluation forward dependencies of insns in chain given
       by two parameter values (head and tail correspondingly).  */
    void (* dependencies_evaluation_hook) (rtx, rtx);

    /* The following member value is a pointer to a function returning
       nonzero if we should use DFA based scheduling.  The default is
       to use the old pipeline scheduler.  */
    int (* use_dfa_pipeline_interface) (void);
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
       the members result in not changing the automaton state when
       the new simulated processor cycle correspondingly starts and
       finishes.  */
    void (* init_dfa_pre_cycle_insn) (void);
    rtx (* dfa_pre_cycle_insn) (void);
    void (* init_dfa_post_cycle_insn) (void);
    rtx (* dfa_post_cycle_insn) (void);
    /* The following member value is a pointer to a function returning value
       which defines how many insns in queue `ready' will we try for
       multi-pass scheduling.  if the member value is nonzero and the
       function returns positive value, the DFA based scheduler will make
       multi-pass scheduling for the first cycle.  In other words, we will
       try to choose ready insn which permits to start maximum number of
       insns on the same cycle.  */
    int (* first_cycle_multipass_dfa_lookahead) (void);
    /* The following member value is pointer to a function controlling
       what insns from the ready insn queue will be considered for the
       multipass insn scheduling.  If the hook returns zero for insn
       passed as the parameter, the insn will be not chosen to be
       issued.  */
    int (* first_cycle_multipass_dfa_lookahead_guard) (rtx);
    /* The following member value is pointer to a function called by
       the insn scheduler before issuing insn passed as the third
       parameter on given cycle.  If the hook returns nonzero, the
       insn is not issued on given processors cycle.  Instead of that,
       the processor cycle is advanced.  If the value passed through
       the last parameter is zero, the insn ready queue is not sorted
       on the new cycle start as usually.  The first parameter passes
       file for debugging output.  The second one passes the scheduler
       verbose level of the debugging output.  The forth and the fifth
       parameter values are correspondingly processor cycle on which
       the previous insn has been issued and the current processor
       cycle.  */
    int (* dfa_new_cycle) (FILE *, int, rtx, int, int, int *);
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
    void (* init_dfa_bubbles) (void);
    rtx (* dfa_bubble) (int);
    /* The following member value is a pointer to a function called
       by the insn scheduler.  It should return true if there exists a
       dependence which is considered costly by the target, between 
       the insn passed as the first parameter, and the insn passed as 
       the second parameter.  The third parameter is the INSN_DEPEND 
       link that represents the dependence between the two insns.  The
       fourth argument is the cost of the dependence as estimated by
       the scheduler.  The last argument is the distance in cycles 
       between the already scheduled insn (first parameter) and the
       the second insn (second parameter).  */
    bool (* is_costly_dependence) (rtx, rtx, rtx, int, int);
  } sched;

  /* Given two decls, merge their attributes and return the result.  */
  tree (* merge_decl_attributes) (tree, tree);

  /* Given two types, merge their attributes and return the result.  */
  tree (* merge_type_attributes) (tree, tree);

  /* Table of machine attributes and functions to handle them.
     Ignored if NULL.  */
  const struct attribute_spec *attribute_table;

  /* Return zero if the attributes on TYPE1 and TYPE2 are incompatible,
     one if they are compatible and two if they are nearly compatible
     (which causes a warning to be generated).  */
  int (* comp_type_attributes) (tree type1, tree type2);

  /* Assign default attributes to the newly defined TYPE.  */
  void (* set_default_type_attributes) (tree type);

  /* Insert attributes on the newly created DECL.  */
  void (* insert_attributes) (tree decl, tree *attributes);

  /* Return true if FNDECL (which has at least one machine attribute)
     can be inlined despite its machine attributes, false otherwise.  */
  bool (* function_attribute_inlinable_p) (tree fndecl);

  /* Return true if bitfields in RECORD_TYPE should follow the
     Microsoft Visual C++ bitfield layout rules.  */
  bool (* ms_bitfield_layout_p) (tree record_type);

  /* Set up target-specific built-in functions.  */
  void (* init_builtins) (void);

  /* Expand a target-specific builtin.  */
  rtx (* expand_builtin) (tree exp, rtx target, rtx subtarget,
			  enum machine_mode mode, int ignore);

  /* For a vendor-specific fundamental TYPE, return a pointer to
     a statically-allocated string containing the C++ mangling for
     TYPE.  In all other cases, return NULL.  */
  const char * (* mangle_fundamental_type) (tree type);

  /* Make any adjustments to libfunc names needed for this target.  */
  void (* init_libfuncs) (void);

  /* Given a decl, a section name, and whether the decl initializer
     has relocs, choose attributes for the section.  */
  /* ??? Should be merged with SELECT_SECTION and UNIQUE_SECTION.  */
  unsigned int (* section_type_flags) (tree, const char *, int);

  /* True if new jumps cannot be created, to replace existing ones or
     not, at the current point in the compilation.  */
  bool (* cannot_modify_jumps_p) (void);

  /* Return a register class for which branch target register
     optimizations should be applied.  */
  int (* branch_target_register_class) (void);

  /* Return true if branch target register optimizations should include
     callee-saved registers that are not already live during the current
     function.  AFTER_PE_GEN is true if prologues and epilogues have
     already been generated.  */
  bool (* branch_target_register_callee_saved) (bool after_pe_gen);

  /* True if the constant X cannot be placed in the constant pool.  */
  bool (* cannot_force_const_mem) (rtx);

  /* True if the insn X cannot be duplicated.  */
  bool (* cannot_copy_insn_p) (rtx);

  /* Given an address RTX, undo the effects of LEGITIMIZE_ADDRESS.  */
  rtx (* delegitimize_address) (rtx);

  /* True if it is OK to do sibling call optimization for the specified
     call expression EXP.  DECL will be the called function, or NULL if
     this is an indirect call.  */
  bool (*function_ok_for_sibcall) (tree decl, tree exp);

  /* True if EXP should be placed in a "small data" section.  */
  bool (* in_small_data_p) (tree);

  /* True if EXP names an object for which name resolution must resolve
     to the current module.  */
  bool (* binds_local_p) (tree);

  /* Do something target-specific to record properties of the DECL into
     the associated SYMBOL_REF.  */
  void (* encode_section_info) (tree, rtx, int);

  /* Undo the effects of encode_section_info on the symbol string.  */
  const char * (* strip_name_encoding) (const char *);

  /* True if MODE is valid for a pointer in __attribute__((mode("MODE"))).  */
  bool (* valid_pointer_mode) (enum machine_mode mode);

  /* True if a vector is opaque.  */
  bool (* vector_opaque_p) (tree);

  /* Compute a (partial) cost for rtx X.  Return true if the complete
     cost has been computed, and false if subexpressions should be
     scanned.  In either case, *TOTAL contains the cost result.  */
  /* Note that CODE and OUTER_CODE ought to be RTX_CODE, but that's
     not necessarily defined at this point.  */
  bool (* rtx_costs) (rtx x, int code, int outer_code, int *total);

  /* Compute the cost of X, used as an address.  Never called with
     invalid addresses.  */
  int (* address_cost) (rtx x);

  /* Given a register, this hook should return a parallel of registers
     to represent where to find the register pieces.  Define this hook
     if the register and its mode are represented in Dwarf in
     non-contiguous locations, or if the register should be
     represented in more than one register in Dwarf.  Otherwise, this
     hook should return NULL_RTX.  */
  rtx (* dwarf_register_span) (rtx);

  /* Fetch the fixed register(s) which hold condition codes, for
     targets where it makes sense to look for duplicate assignments to
     the condition codes.  This should return true if there is such a
     register, false otherwise.  The arguments should be set to the
     fixed register numbers.  Up to two condition code registers are
     supported.  If there is only one for this target, the int pointed
     at by the second argument should be set to -1.  */
  bool (* fixed_condition_code_regs) (unsigned int *, unsigned int *);

  /* If two condition code modes are compatible, return a condition
     code mode which is compatible with both, such that a comparison
     done in the returned mode will work for both of the original
     modes.  If the condition code modes are not compatible, return
     VOIDmode.  */
  enum machine_mode (* cc_modes_compatible) (enum machine_mode,
					     enum machine_mode);

  /* Do machine-dependent code transformations.  Called just before
     delayed-branch scheduling.  */
  void (* machine_dependent_reorg) (void);

  /* Create the __builtin_va_list type.  */
  tree (* build_builtin_va_list) (void);

  /* Validity-checking routines for PCH files, target-specific.
     get_pch_validity returns a pointer to the data to be stored,
     and stores the size in its argument.  pch_valid_p gets the same
     information back and returns NULL if the PCH is valid,
     or an error message if not.
  */
  void * (* get_pch_validity) (size_t *);
  const char * (* pch_valid_p) (const void *, size_t);

  /* Functions relating to calls - argument passing, returns, etc.  */
  struct calls {
    bool (*promote_function_args) (tree fntype);
    bool (*promote_function_return) (tree fntype);
    bool (*promote_prototypes) (tree fntype);
    rtx (*struct_value_rtx) (tree fndecl, int incoming);
    bool (*return_in_memory) (tree type, tree fndecl);
    bool (*return_in_msb) (tree type);
    rtx (*expand_builtin_saveregs) (void);
    /* Returns pretend_argument_size.  */
    void (*setup_incoming_varargs) (CUMULATIVE_ARGS *ca, enum machine_mode mode,
				    tree type, int *pretend_arg_size,
				    int second_time);
    bool (*strict_argument_naming) (CUMULATIVE_ARGS *ca);
    /* Returns true if we should use SETUP_INCOMING_VARARGS and/or
       targetm.calls.strict_argument_naming().  */
    bool (*pretend_outgoing_varargs_named) (CUMULATIVE_ARGS *ca);

    /* Given a complex type T, return true if a parameter of type T
       should be passed as two scalars.  */
    bool (* split_complex_arg) (tree type);
  } calls;

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

  /* True if #NO_APP should be emitted at the beginning of
     assembly output.  */
  bool file_start_app_off;

  /* True if output_file_directive should be called for main_input_filename
     at the beginning of assembly output.  */
  bool file_start_file_directive;

  /* Leave the boolean fields at the end.  */
};

extern struct gcc_target targetm;
