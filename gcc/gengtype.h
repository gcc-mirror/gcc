/* Process source files and output type information.
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#ifndef GCC_GENGTYPE_H
#define GCC_GENGTYPE_H

/* A file position, mostly for error messages.  
   The FILE element may be compared using pointer equality.  */
struct fileloc {
  const char *file;
  int line;
};

/* Data types handed around within, but opaque to, the lexer and parser.  */
typedef struct pair *pair_p;
typedef struct type *type_p;
typedef struct options *options_p;

/* Variables used to communicate between the lexer and the parser.  */
extern int lexer_toplevel_done;
extern struct fileloc lexer_line;

/* Print an error message.  */
extern void error_at_line 
  (struct fileloc *pos, const char *msg, ...) ATTRIBUTE_PRINTF_2;

/* Constructor routines for types.  */
extern void do_typedef (const char *s, type_p t, struct fileloc *pos);
extern void do_scalar_typedef (const char *s, struct fileloc *pos);
extern type_p resolve_typedef (const char *s, struct fileloc *pos);
extern type_p new_structure (const char *name, int isunion,
			     struct fileloc *pos, pair_p fields,
			     options_p o);
extern type_p find_structure (const char *s, int isunion);
extern type_p create_scalar_type (const char *name);
extern type_p create_pointer (type_p t);
extern type_p create_array (type_p t, const char *len);
extern options_p create_option (options_p, const char *name, const void *info);
extern options_p create_nested_ptr_option (options_p, type_p t,
					   const char *from, const char *to);
extern pair_p create_field_at (pair_p next, type_p type, const char *name,
			       options_p opt, struct fileloc *pos);
extern type_p adjust_field_type (type_p, options_p);
extern void note_variable (const char *s, type_p t, options_p o,
			   struct fileloc *pos);
extern void note_def_vec (const char *typename, bool is_scalar,
			  struct fileloc *pos);
extern void note_def_vec_alloc (const char *type, const char *astrat,
				struct fileloc *pos);

/* Lexer and parser routines, most automatically generated.  */
extern int yylex (void);
extern void yyerror (const char *);
extern int yyparse (void);
extern void parse_file (const char *name);

#endif
