/* Definitions of target machine for GNU compiler.  System/370 version.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Jan Stein (jan@cd.chalmers.se).
   Modified for OS/390 LanguageEnvironment C by Dave Pitts (dpitts@cozx.com)
   Hacked for Linux-ELF/390 by Linas Vepstas (linas@linas.org)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_I370_PROTOS_H
#define GCC_I370_PROTOS_H

extern void override_options (void);

#ifdef RTX_CODE
extern int i370_branch_dest (rtx);
extern int i370_branch_length (rtx);
extern int i370_short_branch (rtx);
extern int s_operand (rtx, enum machine_mode);
extern int r_or_s_operand (rtx, enum machine_mode);
extern int unsigned_jump_follows_p (rtx);
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern int handle_pragma (int (*)(void), void (*)(int), const char *);
#endif /* TREE_CODE */

extern void mvs_add_label (int);
extern int mvs_check_label (int);
extern int mvs_check_page (FILE *, int, int);
extern int mvs_function_check (const char *);
extern void mvs_add_alias (const char *, const char *, int);
extern int mvs_need_alias (const char *);
extern int mvs_get_alias (const char *, char *);
extern int mvs_check_alias (const char *, char *);
extern void check_label_emit (void);
extern void mvs_free_label_list (void);

extern void i370_pr_map (struct cpp_reader *);

#endif /* ! GCC_I370_PROTOS_H */
