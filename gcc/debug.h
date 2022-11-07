/* Debug hooks for GCC.
   Copyright (C) 2001-2022 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_DEBUG_H
#define GCC_DEBUG_H

/* This structure contains hooks for the debug information output
   functions, accessed through the global instance debug_hooks set in
   toplev.cc according to command line options.  */
/* WARNING: Do not add new debug hook targets - DWARF will be the only
   way to speak debug to the middle-end once we are able to get rid of
   the remaining targets.  If you need alternate output formats instead
   generate them off the DWARF representation.  */
struct gcc_debug_hooks
{
  /* Initialize debug output.  MAIN_FILENAME is the name of the main
     input file.  */
  void (* init) (const char *main_filename);

  /* Output debug symbols.  */
  void (* finish) (const char *main_filename);

  /* Run cleanups necessary after early debug generation.  */
  void (* early_finish) (const char *main_filename);

  /* Called from cgraph_optimize before starting to assemble
     functions/variables/toplevel asms.  */
  void (* assembly_start) (void);

  /* Macro defined on line LINE with name and expansion TEXT.  */
  void (* define) (unsigned int line, const char *text);

  /* MACRO undefined on line LINE.  */
  void (* undef) (unsigned int line, const char *macro);

  /* Record the beginning of a new source file FILE from LINE number
     in the previous one.  */
  void (* start_source_file) (unsigned int line, const char *file);

  /* Record the resumption of a source file.  LINE is the line number
     in the source file we are returning to.  */
  void (* end_source_file) (unsigned int line);

  /* Record the beginning of block N, counting from 1 and not
     including the function-scope block, at LINE.  */
  void (* begin_block) (unsigned int line, unsigned int n);

  /* Record the end of a block.  Arguments as for begin_block.  */
  void (* end_block) (unsigned int line, unsigned int n);

  /* Returns nonzero if it is appropriate not to emit any debugging
     information for BLOCK, because it doesn't contain any
     instructions.  This may not be the case for blocks containing
     nested functions, since we may actually call such a function even
     though the BLOCK information is messed up.  Defaults to true.  */
  bool (* ignore_block) (const_tree);

  /* Record a source file location at (FILE, LINE, COLUMN, DISCRIMINATOR).  */
  void (* source_line) (unsigned int line, unsigned int column,
			const char *file, int discriminator, bool is_stmt);

  /* Record a source file location for a DECL_IGNORED_P function.  */
  void (* set_ignored_loc) (unsigned int line, unsigned int column,
			    const char *file);

  /* Called at start of prologue code.  LINE is the first line in the
     function.  */
  void (* begin_prologue) (unsigned int line, unsigned int column,
			   const char *file);

  /* Called at end of prologue code.  LINE is the first line in the
     function.  */
  void (* end_prologue) (unsigned int line, const char *file);

  /* Called at beginning of epilogue code.  */
  void (* begin_epilogue) (unsigned int line, const char *file);

  /* Record end of epilogue code.  */
  void (* end_epilogue) (unsigned int line, const char *file);

  /* Called at start of function DECL, before it is declared.  */
  void (* begin_function) (tree decl);

  /* Record end of function.  LINE is highest line number in function.  */
  void (* end_function) (unsigned int line);

  /* Register UNIT as the main translation unit.  Called from front-ends when
     they create their main translation unit.  */
  void (* register_main_translation_unit) (tree);

  /* Debug information for a function DECL.  This might include the
     function name (a symbol), its parameters, and the block that
     makes up the function's body, and the local variables of the
     function.

     This is only called for FUNCTION_DECLs.  It is part of the late
     debug pass and is called from rest_of_handle_final.

     Location information is available at this point.

     See the documentation for early_global_decl and late_global_decl
     for other entry points into the debugging back-ends for DECLs.  */
  void (* function_decl) (tree decl);

  /* Debug information for a global DECL.  Called from the parser
     after the parsing process has finished.

     This gets called for both variables and functions.

     Location information is not available at this point, but it is a
     good probe point to get access to symbols before they get
     optimized away.

     This hook may be called on VAR_DECLs or FUNCTION_DECLs.  It is up
     to the hook to use what it needs.  */
  void (* early_global_decl) (tree decl);

  /* Augment debug information generated by early_global_decl with
     more complete debug info (if applicable).  Called from toplev.cc
     after the compilation proper has finished and cgraph information
     is available.

     This gets called for both variables and functions.

     Location information is usually available at this point, unless
     the hook is being called for a decl that has been optimized away.

     This hook may be called on VAR_DECLs or FUNCTION_DECLs.  It is up
     to the hook to use what it needs.  */
  void (* late_global_decl) (tree decl);

  /* Debug information for a type DECL.  Called from toplev.cc after
     compilation proper, also from various language front ends to
     record built-in types.  The second argument is properly a
     boolean, which indicates whether or not the type is a "local"
     type as determined by the language.  (It's not a boolean for
     legacy reasons.)  */
  void (* type_decl) (tree decl, int local);

  /* Debug information for imported modules and declarations.  */
  void (* imported_module_or_decl) (tree decl, tree name,
				    tree context, bool child,
				    bool implicit);

  /* Return true if a DIE for the tree is available and return a symbol
     and offset that can be used to refer to it externally.  */
  bool (* die_ref_for_decl) (tree, const char **, unsigned HOST_WIDE_INT *);

  /* Early debug information for the tree is available at symbol plus
     offset externally.  */
  void (* register_external_die) (tree, const char *, unsigned HOST_WIDE_INT);

  /* DECL is an inline function, whose body is present, but which is
     not being output at this point.  */
  void (* deferred_inline_function) (tree decl);

  /* DECL is an inline function which is about to be emitted out of
     line.  The hook is useful to, e.g., emit abstract debug info for
     the inline before it gets mangled by optimization.  */
  void (* outlining_inline_function) (tree decl);

  /* Called from final_scan_insn for any CODE_LABEL insn whose
     LABEL_NAME is non-null.  */
  void (* label) (rtx_code_label *);

  /* Called after the start and before the end of writing a PCH file.
     The parameter is 0 if after the start, 1 if before the end.  */
  void (* handle_pch) (unsigned int);

  /* Called from final_scan_insn for any NOTE_INSN_VAR_LOCATION note.  */
  void (* var_location) (rtx_insn *);

  /* Called from final_scan_insn for any NOTE_INSN_INLINE_ENTRY note.  */
  void (* inline_entry) (tree block);

  /* Called from finalize_size_functions for size functions so that their body
     can be encoded in the debug info to describe the layout of variable-length
     structures.  */
  void (* size_function) (tree decl);

  /* Called from final_scan_insn if there is a switch between hot and cold
     text sections.  */
  void (* switch_text_section) (void);

  /* Called from grokdeclarator.  Replaces the anonymous name with the
     type name.  */
  void (* set_name) (tree, tree);

  /* This is 1 if the debug writer wants to see start and end commands for the
     main source files, and 0 otherwise.  */
  int start_end_main_source_file;

  /* The type of symtab field used by these debug hooks.  This is one
     of the TYPE_SYMTAB_IS_xxx values defined in tree.h.  */
  int tree_type_symtab_field;
};

extern const struct gcc_debug_hooks *debug_hooks;

/* The do-nothing hooks.  */
extern void debug_nothing_void (void);
extern void debug_nothing_charstar (const char *);
extern void debug_nothing_int_int_charstar (unsigned int, unsigned int,
					    const char *);
extern void debug_nothing_int_charstar (unsigned int, const char *);
extern void debug_nothing_int_int_charstar_int_bool (unsigned int,
						     unsigned int,
						     const char *,
						     int, bool);
extern void debug_nothing_int (unsigned int);
extern void debug_nothing_int_int (unsigned int, unsigned int);
extern void debug_nothing_tree (tree);
extern void debug_nothing_tree_tree (tree, tree);
extern void debug_nothing_tree_int (tree, int);
extern void debug_nothing_tree_tree_tree_bool_bool (tree, tree, tree,
						    bool, bool);
extern bool debug_true_const_tree (const_tree);
extern void debug_nothing_rtx_insn (rtx_insn *);
extern void debug_nothing_rtx_code_label (rtx_code_label *);
extern bool debug_false_tree_charstarstar_uhwistar (tree, const char **,
						    unsigned HOST_WIDE_INT *);
extern void debug_nothing_tree_charstar_uhwi (tree, const char *,
					      unsigned HOST_WIDE_INT);

/* Hooks for various debug formats.  */
extern const struct gcc_debug_hooks do_nothing_debug_hooks;
extern const struct gcc_debug_hooks xcoff_debug_hooks;
extern const struct gcc_debug_hooks dwarf2_debug_hooks;
extern const struct gcc_debug_hooks dwarf2_lineno_debug_hooks;
extern const struct gcc_debug_hooks vmsdbg_debug_hooks;

/* Dwarf2 frame information.  */

extern void dwarf2out_begin_prologue (unsigned int, unsigned int,
				      const char *);
extern void dwarf2out_vms_end_prologue (unsigned int, const char *);
extern void dwarf2out_vms_begin_epilogue (unsigned int, const char *);
extern void dwarf2out_end_epilogue (unsigned int, const char *);
extern void dwarf2out_frame_finish (void);
extern bool dwarf2out_do_eh_frame (void);
extern bool dwarf2out_do_frame (void);
extern bool dwarf2out_do_cfi_asm (void);
extern void dwarf2out_switch_text_section (void);
extern bool dwarf2out_default_as_loc_support (void);
extern bool dwarf2out_default_as_locview_support (void);

/* For -fdump-go-spec.  */

extern const struct gcc_debug_hooks *
dump_go_spec_init (const char *, const struct gcc_debug_hooks *);

/* Instance discriminator mapping table.  See final.cc.  */
typedef hash_map<const_tree, int> decl_to_instance_map_t;
extern decl_to_instance_map_t *decl_to_instance_map;

/* Allocate decl_to_instance_map with COUNT slots to begin wtih, if it
 * hasn't been allocated yet.  */

static inline decl_to_instance_map_t *
maybe_create_decl_to_instance_map (int count = 13)
{
  if (!decl_to_instance_map)
    decl_to_instance_map = new decl_to_instance_map_t (count);
  return decl_to_instance_map;
}

#endif /* !GCC_DEBUG_H  */
