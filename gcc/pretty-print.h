/* Various declarations for language-independent pretty-print subroutines.
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_PRETTY_PRINT_H
#define GCC_PRETTY_PRINT_H

#include "diagnostic.h"

typedef struct pretty_print_info pretty_print_info;

/* The type of pretty-printer flags passed to clients.  */
typedef unsigned int pp_flags;

enum pp_padding
{
  pp_none, pp_before, pp_after
};

struct pretty_print_info
{
  /* The entity to pretty-print.  */
  tree entity;
  pp_flags flags;
  /* Where to put whitespace around the entity being formatted.  */
  enum pp_padding padding;
  /* Where we print external representation of ENTITY.  */
  output_buffer *buffer;
};


#define pp_sorry_for_unsupported_tree(PPI, T) \
  output_verbatim ((PPI)->buffer, "\nsorry: `%s' not supported by %s\n",\
                   tree_code_name[(int) TREE_CODE (T)], __FUNCTION__)

#define pp_left_paren(PPI)    output_add_character ((PPI)->buffer, '(')
#define pp_right_paren(PPI)   output_add_character ((PPI)->buffer, ')')
#define pp_left_bracket(PPI)  output_add_character ((PPI)->buffer, '[')
#define pp_right_bracket(PPI) output_add_character ((PPI)->buffer, '[')
#define pp_semi_colon(PPI)    output_add_character ((PPI)->buffer, ';')
#define pp_comma(PPI)         output_add_string ((PPI)->buffer, ", ")
#define pp_dot(PPI)           output_add_character ((PPI)->buffer, '.')
#define pp_colon(PPI)         output_add_character ((PPI)->buffer, ':')
#define pp_colon_colon(PPI)   output_add_string ((PPI)->buffer, "::")
#define pp_quote(PPI)         output_add_character ((PPI)->buffer, '\'')
#define pp_backquote(PPI)     output_add_character ((PPI)->buffer, '`')
#define pp_doublequote(PPI)   output_add_character ((PPI)->buffer, '"')


#endif /* GCC_PRETTY_PRINT_H */
