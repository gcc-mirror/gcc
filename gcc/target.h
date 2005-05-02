/* Data structure definitions for a generic GCC target.
   Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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

#ifndef GCC_TARGET_H
#define GCC_TARGET_H

#include "tm.h"
#include "insn-modes.h"

struct stdarg_info;

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

    /* Output code that will emit a label for unwind info, if this
       target requires such labels.  Second argument is the decl the
       unwind info is associated with, third is a boolean: true if
       this is for exception handling, fourth is a boolean: true if
       this is only a placeholder for an omitted FDE.  */
    void (* unwind_label) (FILE *, tree, int, int);

    /* Emit any directives required to unwind this instruction.  */
    void (* unwind_emit) (FILE *, rtx);

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

    /* Tell assembler to change to section NAME with attributes FLAGS.
       If DECL is non-NULL, it is the VAR_DECL or FUNCTION_DECL with
       which this section is associated.  */
    void (* named_section) (const char *name, unsigned int flags, tree decl);

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

    /* Tell assembler to switch to the readonly data section associated
       with function DECL.  */
    void (* function_rodata_section) (tree);

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

     /* Output an assembler directive to mark decl live. This instructs
	linker to not dead code strip this symbol.  */
    void (*mark_decl_preserved) (const char *);

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

    /* Initialize machine-dependent function while scheduling code.  */
    void (* md_init_global) (FILE *, int, int);

    /* Finalize machine-dependent function wide scheduling code.  */
    void (* md_finish_global) (FILE *, int);

    /* Reorder insns in a machine-dependent fashion, in two different
       places.  Default does nothing.  */
    int (* reorder) (FILE *, int, rtx *, int *, int);
    int (* reorder2) (FILE *, int, rtx *, int *, int);

    /* The following member value is a pointer to a function called
       after evaluation forward dependencies of insns in chain given
       by two parameter values (head and tail correspondingly).  */
    void (* dependencies_evaluation_hook) (rtx, rtx);

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
       multi-pass scheduling.  If the member value is nonzero and the
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

  /* Functions relating to vectorization.  */
  struct vectorize
  {
    /* The following member value is a pointer to a function called
       by the vectorizer, and return the decl of the target builtin
       function.  */
    tree (* builtin_mask_for_load) (void);
  } vectorize;

  /* The initial value of target_flags.  */
  int default_target_flags;

  /* Handle target switch CODE (an OPT_* value).  ARG is the argument
     passed to the switch; it is NULL if no argument was.  VALUE is the
     value of ARG if CODE specifies a UInteger option, otherwise it is
     1 if the positive form of the switch was used and 0 if the negative
     form was.  Return true if the switch was valid.  */
  bool (* handle_option) (size_t code, const char *arg, int value);

  /* Return machine mode for filter value.  */
  enum machine_mode (* eh_return_filter_mode) (void);

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

  /* Return true if anonymous bitfields affect structure alignment.  */
  bool (* align_anon_bitfield) (void);

  /* Set up target-specific built-in functions.  */
  void (* init_builtins) (void);

  /* Expand a target-specific builtin.  */
  rtx (* expand_builtin) (tree exp, rtx target, rtx subtarget,
			  enum machine_mode mode, int ignore);

  /* Select a replacement for a target-specific builtin.  This is done
     *before* regular type checking, and so allows the target to implement
     a crude form of function overloading.  The result is a complete
     expression that implements the operation.  */
  tree (*resolve_overloaded_builtin) (tree decl, tree params);

  /* Fold a target-specific builtin.  */
  tree (* fold_builtin) (tree fndecl, tree arglist, bool ignore);
  
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

  /* If shift optabs for MODE are known to always truncate the shift count,
     return the mask that they apply.  Return 0 otherwise.  */
  unsigned HOST_WIDE_INT (* shift_truncation_mask) (enum machine_mode mode);

  /* True if MODE is valid for a pointer in __attribute__((mode("MODE"))).  */
  bool (* valid_pointer_mode) (enum machine_mode mode);

  /* True if MODE is valid for the target.  By "valid", we mean able to
     be manipulated in non-trivial ways.  In particular, this means all
     the arithmetic is supported.  */
  bool (* scalar_mode_supported_p) (enum machine_mode mode);

  /* Similarly for vector modes.  "Supported" here is less strict.  At
     least some operations are supported; need to check optabs or builtins
     for further details.  */
  bool (* vector_mode_supported_p) (enum machine_mode mode);

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

  /* Gimplifies a VA_ARG_EXPR.  */
  tree (* gimplify_va_arg_expr) (tree valist, tree type, tree *pre_p,
				 tree *post_p);

  /* Validity-checking routines for PCH files, target-specific.
     get_pch_validity returns a pointer to the data to be stored,
     and stores the size in its argument.  pch_valid_p gets the same
     information back and returns NULL if the PCH is valid,
     or an error message if not.
  */
  void * (* get_pch_validity) (size_t *);
  const char * (* pch_valid_p) (const void *, size_t);

  /* True if the compiler should give an enum type only as many
     bytes as it takes to represent the range of possible values of
     that type.  */
  bool (* default_short_enums) (void);

  /* This target hook returns an rtx that is used to store the address
     of the current frame into the built-in setjmp buffer.  */
  rtx (* builtin_setjmp_frame_value) (void);

  /* This target hook should add STRING_CST trees for any hard regs
     the port wishes to automatically clobber for an asm.  */
  tree (* md_asm_clobbers) (tree, tree, tree);

  /* This target hook allows the backend to specify a calling convention
     in the debug information.  This function actually returns an
     enum dwarf_calling_convention, but because of forward declarations
     and not wanting to include dwarf2.h everywhere target.h is included
     the function is being declared as an int.  */
  int (* dwarf_calling_convention) (tree);

  /* This target hook allows the backend to emit frame-related insns that
     contain UNSPECs or UNSPEC_VOLATILEs.  The call frame debugging info
     engine will invoke it on insns of the form
       (set (reg) (unspec [...] UNSPEC_INDEX))
     and
       (set (reg) (unspec_volatile [...] UNSPECV_INDEX))
     to let the backend emit the call frame instructions.  */
  void (* dwarf_handle_frame_unspec) (const char *, rtx, int);

  /* Perform architecture specific checking of statements gimplified
     from VA_ARG_EXPR.  LHS is left hand side of MODIFY_EXPR, RHS
     is right hand side.  Returns true if the statements doesn't need
     to be checked for va_list references.  */
  bool (*stdarg_optimize_hook) (struct stdarg_info *ai, tree lhs, tree rhs);

  /* Functions relating to calls - argument passing, returns, etc.  */
  struct calls {
    bool (*promote_function_args) (tree fntype);
    bool (*promote_function_return) (tree fntype);
    bool (*promote_prototypes) (tree fntype);
    rtx (*struct_value_rtx) (tree fndecl, int incoming);
    bool (*return_in_memory) (tree type, tree fndecl);
    bool (*return_in_msb) (tree type);

    /* Return true if a parameter must be passed by reference.  TYPE may
       be null if this is a libcall.  CA may be null if this query is
       from __builtin_va_arg.  */
    bool (*pass_by_reference) (CUMULATIVE_ARGS *ca, enum machine_mode mode,
			       tree type, bool named_arg);

    rtx (*expand_builtin_saveregs) (void);
    /* Returns pretend_argument_size.  */
    void (*setup_incoming_varargs) (CUMULATIVE_ARGS *ca, enum machine_mode mode,
				    tree type, int *pretend_arg_size,
				    int second_time);
    bool (*strict_argument_naming) (CUMULATIVE_ARGS *ca);
    /* Returns true if we should use
       targetm.calls.setup_incoming_varargs() and/or
       targetm.calls.strict_argument_naming().  */
    bool (*pretend_outgoing_varargs_named) (CUMULATIVE_ARGS *ca);

    /* Given a complex type T, return true if a parameter of type T
       should be passed as two scalars.  */
    bool (* split_complex_arg) (tree type);

    /* Return true if type T, mode MODE, may not be passed in registers,
       but must be passed on the stack.  */
    /* ??? This predicate should be applied strictly after pass-by-reference.
       Need audit to verify that this is the case.  */
    bool (* must_pass_in_stack) (enum machine_mode mode, tree t);

    /* Return true if type TYPE, mode MODE, which is passed by reference,
       should have the object copy generated by the callee rather than
       the caller.  It is never called for TYPE requiring constructors.  */
    bool (* callee_copies) (CUMULATIVE_ARGS *ca, enum machine_mode mode,
			    tree type, bool named);

    /* Return zero for arguments passed entirely on the stack or entirely
       in registers.  If passed in both, return the number of bytes passed
       in registers; the balance is therefore passed on the stack.  */
    int (* arg_partial_bytes) (CUMULATIVE_ARGS *ca, enum machine_mode mode,
			       tree type, bool named);

    /* Return the diagnostic message string if function without a prototype
       is not allowed for this 'val' argument; NULL otherwise. */
    const char *(*invalid_arg_for_unprototyped_fn) (tree typelist, 
					     	    tree funcdecl, tree val);
  } calls;

  /* Functions specific to the C++ frontend.  */
  struct cxx {
    /* Return the integer type used for guard variables.  */
    tree (*guard_type) (void);
    /* Return true if only the low bit of the guard should be tested.  */
    bool (*guard_mask_bit) (void);
    /* Returns the size of the array cookie for an array of type.  */
    tree (*get_cookie_size) (tree);
    /* Returns true if the element size should be stored in the
       array cookie.  */
    bool (*cookie_has_size) (void);
    /* Allows backends to perform additional processing when
       deciding if a class should be exported or imported.  */
    int (*import_export_class) (tree, int);
    /* Returns true if constructors and destructors return "this".  */
    bool (*cdtor_returns_this) (void);
    /* Returns true if the key method for a class can be an inline
       function, so long as it is not declared inline in the class
       itself.  Returning true is the behavior required by the Itanium
       C++ ABI.  */
    bool (*key_method_may_be_inline) (void);
    /* DECL is a virtual table, virtual table table, typeinfo object,
       or other similar implicit class data object that will be
       emitted with external linkage in this translation unit.  No ELF
       visibility has been explicitly specified.  If the target needs
       to specify a visibility other than that of the containing class,
       use this hook to set DECL_VISIBILITY and
       DECL_VISIBILITY_SPECIFIED.  */ 
    void (*determine_class_data_visibility) (tree decl);
    /* Returns true (the default) if virtual tables and other
       similar implicit class data objects are always COMDAT if they
       have external linkage.  If this hook returns false, then
       class data for classes whose virtual table will be emitted in
       only one translation unit will not be COMDAT.  */
    bool (*class_data_always_comdat) (void);
    /* Returns true if __aeabi_atexit should be used to register static
       destructors.  */
    bool (*use_aeabi_atexit) (void);
  } cxx;

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

  /* True if #pragma redefine_extname is to be supported.  */
  bool handle_pragma_redefine_extname;

  /* True if #pragma extern_prefix is to be supported.  */
  bool handle_pragma_extern_prefix;

  /* True if the target is allowed to reorder memory accesses unless
     synchronization is explicitly requested.  */
  bool relaxed_ordering;

  /* Leave the boolean fields at the end.  */
};

extern struct gcc_target targetm;

#endif /* GCC_TARGET_H */
