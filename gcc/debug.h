/* Debug hooks for GCC.
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.

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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef GCC_DEBUG_H
#define GCC_DEBUG_H

/* This structure contains hooks for the debug information output
   functions, accessed through the global instance debug_hooks set in
   toplev.c according to command line options.  */
struct gcc_debug_hooks
{
  /* Initialise debug output.  MAIN_FILENAME is the name of the main
     input file.  */
  void (* init) PARAMS ((const char *main_filename));

  /* Output debug symbols.  */
  void (* finish) PARAMS ((const char *main_filename));

  /* Macro defined on line LINE with name and expansion TEXT.  */
  void (* define) PARAMS ((unsigned int line, const char *text));

  /* MACRO undefined on line LINE.  */
  void (* undef) PARAMS ((unsigned int line, const char *macro));

  /* Record the beginning of a new source file FILE from LINE number
     in the previous one.  */
  void (* start_source_file) PARAMS ((unsigned int line, const char *file));

  /* Record the resumption of a source file.  LINE is the line number
     in the source file we are returning to.  */
  void (* end_source_file) PARAMS ((unsigned int line));

  /* Record the beginning of block N, counting from 1 and not
     including the function-scope block, at LINE.  */
  void (* begin_block) PARAMS ((unsigned int line, unsigned int n));

  /* Record the end of a block.  Arguments as for begin_block.  */
  void (* end_block) PARAMS ((unsigned int line, unsigned int n));

  /* Returns nonzero if it is appropriate not to emit any debugging
     information for BLOCK, because it doesn't contain any
     instructions.  This may not be the case for blocks containing
     nested functions, since we may actually call such a function even
     though the BLOCK information is messed up.  Defaults to true.  */
  bool (* ignore_block) PARAMS ((tree));

  /* Record a source file location at (FILE, LINE).  */
  void (* source_line) PARAMS ((unsigned int line, const char *file));

  /* Called at start of prologue code.  LINE is the first line in the
     function.  This has been given the same prototype as source_line,
     so that the source_line hook can be substituted if appropriate.  */
  void (* begin_prologue) PARAMS ((unsigned int line, const char *file));

  /* Called at end of prologue code.  LINE is the first line in the
     function.  */
  void (* end_prologue) PARAMS ((unsigned int line));

  /* Record end of epilogue code.  */
  void (* end_epilogue) PARAMS ((void));

  /* Called at start of function DECL, before it is declared.  */
  void (* begin_function) PARAMS ((tree decl));

  /* Record end of function.  LINE is highest line number in function.  */
  void (* end_function) PARAMS ((unsigned int line));

  /* Debug information for a function DECL.  This might include the
     function name (a symbol), its parameters, and the block that
     makes up the function's body, and the local variables of the
     function.  */
  void (* function_decl) PARAMS ((tree decl));

  /* Debug information for a global DECL.  Called from toplev.c after
     compilation proper has finished.  */
  void (* global_decl) PARAMS ((tree decl));

  /* DECL is an inline function, whose body is present, but which is
     not being output at this point.  */
  void (* deferred_inline_function) PARAMS ((tree decl));

  /* DECL is an inline function which is about to be emitted out of
     line.  The hook is useful to, e.g., emit abstract debug info for
     the inline before it gets mangled by optimization.  */
  void (* outlining_inline_function) PARAMS ((tree decl));

  /* Called from final_scan_insn for any CODE_LABEL insn whose
     LABEL_NAME is non-null.  */
  void (* label) PARAMS ((rtx));
};

extern struct gcc_debug_hooks *debug_hooks;

/* The do-nothing hooks.  */
extern void debug_nothing_void
  PARAMS ((void));
extern void debug_nothing_charstar
  PARAMS ((const char *));
extern void debug_nothing_int_charstar
  PARAMS ((unsigned int, const char *));
extern void debug_nothing_int
  PARAMS ((unsigned int));
extern void debug_nothing_int_int
  PARAMS ((unsigned int, unsigned int));
extern void debug_nothing_tree
  PARAMS ((tree));
extern bool debug_true_tree
  PARAMS ((tree));
extern void debug_nothing_rtx
  PARAMS ((rtx));

/* Hooks for various debug formats.  */
extern struct gcc_debug_hooks do_nothing_debug_hooks;
extern struct gcc_debug_hooks dbx_debug_hooks;
extern struct gcc_debug_hooks sdb_debug_hooks;
extern struct gcc_debug_hooks xcoff_debug_hooks;
extern struct gcc_debug_hooks dwarf_debug_hooks;
extern struct gcc_debug_hooks dwarf2_debug_hooks;
extern struct gcc_debug_hooks vmsdbg_debug_hooks;

/* Dwarf2 frame information.  */

extern void dwarf2out_begin_prologue	PARAMS ((unsigned int, const char *));
extern void dwarf2out_end_epilogue	PARAMS ((void));
extern void dwarf2out_frame_init	PARAMS ((void));
extern void dwarf2out_frame_finish	PARAMS ((void));
/* Decide whether we want to emit frame unwind information for the current
   translation unit.  */
extern int dwarf2out_do_frame		PARAMS ((void));

/* When writing VMS debug info, output label after the prologue of the
   function.  */
extern void vmsdbgout_after_prologue	PARAMS ((void));



#endif /* !GCC_DEBUG_H  */
