/* toplev.h - Various declarations for functions found in toplev.c
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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

#ifndef __GCC_TOPLEV_H__
#define __GCC_TOPLEV_H__

#ifdef ANSI_PROTOTYPES
union tree_node;
struct rtx_def;
#endif

extern int count_error			PROTO ((int));
extern void strip_off_ending		PROTO ((char *, int));
extern void print_time			PROTO ((char *, int));
extern void debug_start_source_file	PROTO ((char *));
extern void debug_end_source_file	PROTO ((unsigned));
extern void debug_define		PROTO ((unsigned, char *));
extern void debug_undef			PROTO ((unsigned, char *));
extern void fatal			PVPROTO ((char *, ...))
  ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
extern void fatal_io_error		PROTO ((char *)) ATTRIBUTE_NORETURN;
extern void pfatal_with_name		PROTO ((char *)) ATTRIBUTE_NORETURN;
extern void fatal_insn_not_found	PROTO ((struct rtx_def *))
  ATTRIBUTE_NORETURN;
extern void fatal_insn			PROTO ((char *, struct rtx_def *))
  ATTRIBUTE_NORETURN;
extern void warning			PVPROTO ((char *, ...))
						ATTRIBUTE_PRINTF_1;
extern void error			PVPROTO ((char *, ...))
						ATTRIBUTE_PRINTF_1;
extern void pedwarn			PVPROTO ((char *, ...))
						ATTRIBUTE_PRINTF_1;
extern void pedwarn_with_file_and_line	PVPROTO ((char *, int, char *, ...))
						ATTRIBUTE_PRINTF_3;
extern void warning_with_file_and_line	PVPROTO ((char *, int, char *, ...))
						ATTRIBUTE_PRINTF_3;
extern void error_with_file_and_line	PVPROTO ((char *, int, char *, ...))
						ATTRIBUTE_PRINTF_3;
extern void sorry			PVPROTO ((char *s, ...))
						ATTRIBUTE_PRINTF_1;
extern void really_sorry		PVPROTO((char *s, ...))
  ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
extern void default_print_error_function PROTO ((char *));
extern void report_error_function	PROTO ((char *));

extern void rest_of_decl_compilation	PROTO ((union tree_node *, char *, int, int));
extern void rest_of_type_compilation	PROTO ((union tree_node *, int));
extern void rest_of_compilation		PROTO ((union tree_node *));
extern void pedwarn_with_decl		PVPROTO ((union tree_node *, char *, ...));
extern void warning_with_decl		PVPROTO ((union tree_node *, char *, ...));
extern void error_with_decl		PVPROTO ((union tree_node *, char *, ...));
extern void announce_function		PROTO ((union tree_node *));

extern void error_for_asm		PVPROTO((struct rtx_def *, char *, ...))
						ATTRIBUTE_PRINTF_2;
extern void warning_for_asm		PVPROTO((struct rtx_def *, char *, ...))
						ATTRIBUTE_PRINTF_2;
#if defined (_JBLEN) || defined (setjmp)
extern void set_float_handler PROTO((jmp_buf));
extern int push_float_handler PROTO((jmp_buf, jmp_buf));
extern void pop_float_handler PROTO((int, jmp_buf));
#endif

#ifdef BUFSIZ
extern void output_quoted_string	PROTO ((FILE *, char *));
extern void output_file_directive	PROTO ((FILE *, char *));
#endif

extern void fancy_abort			PROTO ((void)) ATTRIBUTE_NORETURN;
extern void do_abort			PROTO ((void)) ATTRIBUTE_NORETURN;
extern void botch			PROTO ((char *)) ATTRIBUTE_NORETURN;

#endif /* __GCC_TOPLEV_H */
