/* Definitions of target machine for GNU compiler.  System/370 version.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Jan Stein (jan@cd.chalmers.se).
   Modified for OS/390 LanguageEnvironment C by Dave Pitts (dpitts@cozx.com)
   Hacked for Linux-ELF/390 by Linas Vepstas (linas@linas.org)

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

#ifndef GCC_I370_PROTOS_H
#define GCC_I370_PROTOS_H

extern void override_options PARAMS ((void));

#ifdef RTX_CODE
extern int i370_branch_dest PARAMS ((rtx));
extern int i370_branch_length PARAMS ((rtx));
extern int i370_short_branch PARAMS ((rtx));
extern int s_operand PARAMS ((rtx, enum machine_mode));
extern int r_or_s_operand PARAMS ((rtx, enum machine_mode));
extern int unsigned_jump_follows_p PARAMS ((rtx));
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern int handle_pragma PARAMS ((int (*)(void), void (*)(int), const char *));
#endif /* TREE_CODE */

extern char mvs_map_char PARAMS ((int));
extern void mvs_add_label PARAMS ((int));
extern int mvs_check_label PARAMS ((int));
extern int mvs_check_page PARAMS ((FILE *, int, int));
extern int mvs_function_check PARAMS ((const char *));
extern void mvs_add_alias PARAMS ((const char *, const char *, int));
extern int mvs_need_alias PARAMS ((const char *));
extern int mvs_get_alias PARAMS ((const char *, char *));
extern int mvs_check_alias PARAMS ((const char *, char *));
extern void check_label_emit PARAMS ((void));
extern void mvs_free_label_list PARAMS ((void));

#ifdef GCC_C_PRAGMA_H
extern void i370_pr_map PARAMS ((cpp_reader *));
#endif

#endif /* ! GCC_I370_PROTOS_H */
