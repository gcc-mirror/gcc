/* toplev.h - Various declarations for functions found in toplev.c
   Copyright (C) 1998 Free Software Foundation, Inc.
 */

#ifndef __GCC_TOPLEV_H__
#define __GCC_TOPLEV_H__

#ifdef __STDC__
union tree_node;
struct rtx_def;
#endif

extern int count_error			PROTO ((int));
extern void strip_off_ending		PROTO ((char *, int));
extern void print_time			PROTO ((char *, int));
extern int get_run_time			PROTO ((void));
extern void debug_start_source_file	PROTO ((char *));
extern void debug_end_source_file	PROTO ((unsigned));
extern void debug_define		PROTO ((unsigned, char *));
extern void debug_undef			PROTO ((unsigned, char *));
extern void fatal			PVPROTO ((char *, ...))
						ATTRIBUTE_PRINTF_1;
extern void fatal_io_error		PROTO ((char *));
extern void pfatal_with_name		PROTO ((char *));
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
#ifdef _JBLEN
extern void set_float_handler PROTO((jmp_buf));
#endif

#ifdef BUFSIZ
extern void output_quoted_string	PROTO ((FILE *, char *));
extern void output_file_directive	PROTO ((FILE *, char *));
#endif

extern void fancy_abort			PROTO ((void));

#endif /* __GCC_TOPLEV_H */
