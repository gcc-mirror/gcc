/* toplev.h - Various declarations for functions found in toplev.c
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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

extern int read_integral_parameter	PARAMS ((const char *, const char *,
						const int));
extern int count_error			PARAMS ((int));
extern void strip_off_ending		PARAMS ((char *, int));
extern void print_time			PARAMS ((const char *, int));
extern void debug_start_source_file	PARAMS ((char *));
extern void debug_end_source_file	PARAMS ((unsigned));
extern void debug_define		PARAMS ((unsigned, char *));
extern void debug_undef			PARAMS ((unsigned, char *));
extern void debug_ignore_block		PARAMS ((union tree_node *));
extern void fatal			PARAMS ((const char *, ...))
  ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
extern void fatal_io_error		PARAMS ((const char *))
  ATTRIBUTE_NORETURN;
extern void pfatal_with_name		PARAMS ((const char *))
  ATTRIBUTE_NORETURN;
extern void _fatal_insn_not_found	PARAMS ((struct rtx_def *,
						const char *, int,
						const char *))
  ATTRIBUTE_NORETURN;
extern void _fatal_insn			PARAMS ((const char *,
						struct rtx_def *,
						const char *, int,
						const char *))
  ATTRIBUTE_NORETURN;

#if (GCC_VERSION >= 2007)
#define fatal_insn(msgid, insn) \
	_fatal_insn (msgid, insn, __FILE__, __LINE__, __PRETTY_FUNCTION__)
#define fatal_insn_not_found(insn) \
	_fatal_insn_not_found (insn, __FILE__, __LINE__, __PRETTY_FUNCTION__)
#else
#define fatal_insn(msgid, insn) \
	_fatal_insn (msgid, insn, __FILE__, __LINE__, 0)
#define fatal_insn_not_found(insn) \
	_fatal_insn_not_found (insn, __FILE__, __LINE__, 0)
#endif
extern void warning			PARAMS ((const char *, ...))
						ATTRIBUTE_PRINTF_1;
extern void error			PARAMS ((const char *, ...))
						ATTRIBUTE_PRINTF_1;
extern void pedwarn			PARAMS ((const char *, ...))
						ATTRIBUTE_PRINTF_1;
extern void pedwarn_with_file_and_line	PARAMS ((const char *, int,
						  const char *, ...))
  ATTRIBUTE_PRINTF_3;
extern void warning_with_file_and_line	PARAMS ((const char *, int,
						  const char *, ...))
  ATTRIBUTE_PRINTF_3;
extern void error_with_file_and_line	PARAMS ((const char *, int,
						  const char *, ...))
  ATTRIBUTE_PRINTF_3;
extern void sorry			PARAMS ((const char *, ...))
  ATTRIBUTE_PRINTF_1;
extern void really_sorry		PARAMS ((const char *, ...))
  ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
extern void default_print_error_function PARAMS ((const char *));
extern void report_error_function	PARAMS ((const char *));

extern void rest_of_decl_compilation	PARAMS ((union tree_node *,
						const char *, int, int));
extern void rest_of_type_compilation	PARAMS ((union tree_node *, int));
extern void rest_of_compilation		PARAMS ((union tree_node *));

/* The *_with_decl functions aren't suitable for ATTRIBUTE_PRINTF. */
extern void pedwarn_with_decl		PARAMS ((union tree_node *,
						  const char *, ...));
extern void warning_with_decl		PARAMS ((union tree_node *,
						  const char *, ...));
extern void error_with_decl		PARAMS ((union tree_node *,
						  const char *, ...));

extern void announce_function		PARAMS ((union tree_node *));

extern void error_for_asm		PARAMS ((struct rtx_def *,
						 const char *, ...))
  ATTRIBUTE_PRINTF_2;
extern void warning_for_asm		PARAMS ((struct rtx_def *,
						 const char *, ...))
  ATTRIBUTE_PRINTF_2;
#if defined (_JBLEN) || defined (setjmp)
extern void set_float_handler PARAMS ((jmp_buf));
extern int push_float_handler PARAMS ((jmp_buf, jmp_buf));
extern void pop_float_handler PARAMS ((int, jmp_buf));
#endif
extern int do_float_handler PARAMS ((void (*) (PTR), PTR));

#ifdef BUFSIZ
extern void output_quoted_string	PARAMS ((FILE *, const char *));
extern void output_file_directive	PARAMS ((FILE *, const char *));
#endif
extern void do_abort			PARAMS ((void)) ATTRIBUTE_NORETURN;
extern void botch			PARAMS ((const char *))
  ATTRIBUTE_NORETURN;

#ifdef BUFSIZ
extern void fnotice			PARAMS ((FILE *, const char *, ...))
  ATTRIBUTE_PRINTF_2;
#endif

extern int wrapup_global_declarations   PARAMS ((union tree_node **, int));
extern void check_global_declarations   PARAMS ((union tree_node **, int));
extern void note_deferral_of_defined_inline_function PARAMS ((union tree_node *));
extern void set_message_length		PARAMS ((int));
extern int errorcount;
extern int warningcount;
extern int sorrycount;

extern const char *progname;

extern void set_fatal_function PARAMS ((void (*) (const char *, va_list)));
#endif /* __GCC_TOPLEV_H */
