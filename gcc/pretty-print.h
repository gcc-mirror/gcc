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

/* The type of pretty-printer flags passed to clients.  */
typedef unsigned int pp_flags;

typedef enum
{
  pp_none, pp_before, pp_after
} pp_padding;

struct pretty_print_info
{
  /* Where we print external representation of ENTITY.  */
  output_buffer *buffer;
  pp_flags flags;
  /* Where to put whitespace around the entity being formatted.  */
  pp_padding padding;
};

#define pp_left_paren(PPI)      output_add_character (pp_buffer (PPI), '(')
#define pp_right_paren(PPI)     output_add_character (pp_buffer (PPI), ')')
#define pp_left_bracket(PPI)    output_add_character (pp_buffer (PPI), '[')
#define pp_right_bracket(PPI)   output_add_character (pp_buffer (PPI), ']')
#define pp_left_brace(PPI)      output_add_character (pp_buffer (PPI), '{')
#define pp_right_brace(PPI)     output_add_character (pp_buffer (PPI), '}')
#define pp_semicolon(PPI)       output_add_character (pp_buffer (PPI), ';')
#define pp_comma(PPI)           output_add_string (pp_buffer (PPI), ", ")
#define pp_dot(PPI)             output_add_character (pp_buffer (PPI), '.')
#define pp_colon(PPI)           output_add_character (pp_buffer (PPI), ':')
#define pp_colon_colon(PPI)     output_add_string (pp_buffer (PPI), "::")
#define pp_arrow(PPI)           output_add_string (pp_buffer (PPI), "->")
#define pp_equal(PPI)           output_add_character (pp_buffer (PPI), '=')
#define pp_question(PPI)        output_add_character (pp_buffer (PPI), '?')
#define pp_bar(PPI)             output_add_character (pp_buffer (PPI), '|')
#define pp_carret(PPI)          output_add_character (pp_buffer (PPI), '^')
#define pp_ampersand(PPI)       output_add_character (pp_buffer (PPI), '&')
#define pp_less(PPI)            output_add_character (pp_buffer (PPI), '<')
#define pp_greater(PPI)         output_add_character (pp_buffer (PPI), '>')
#define pp_plus(PPI)            output_add_character (pp_buffer (PPI), '+')
#define pp_minus(PPI)           output_add_character (pp_buffer (PPI), '-')
#define pp_star(PPI)            output_add_character (pp_buffer (PPI), '*')
#define pp_slash(PPI)           output_add_character (pp_buffer (PPI), '/')
#define pp_modulo(PPI)          output_add_character (pp_buffer (PPI), '%')
#define pp_exclamation(PPI)     output_add_character (pp_buffer (PPI), '!')
#define pp_complement(PPI)      output_add_character (pp_buffer (PPI), '~')
#define pp_quote(PPI)           output_add_character (pp_buffer (PPI), '\'')
#define pp_backquote(PPI)       output_add_character (pp_buffer (PPI), '`')
#define pp_doublequote(PPI)     output_add_character (pp_buffer (PPI), '"')
#define pp_newline(PPI)         output_add_newline (pp_buffer (PPI))
#define pp_character(PPI, C)    output_add_character (pp_buffer (PPI), C)
#define pp_whitespace(PPI)      output_add_space (pp_buffer (PPI))
#define pp_indentation(PPI)     output_indentation (pp_buffer (PPI))
#define pp_newline_and_indent(PPI, N) \
  do {                                \
    pp_indentation (PPI) += N;        \
    pp_newline (PPI);                 \
  } while (0)
#define pp_separate_with(PPI, C) \
   do {                          \
     pp_character (PPI, C);      \
     pp_whitespace (PPI);        \
   } while (0)
#define pp_format_scalar(PPI, F, S) \
   output_formatted_scalar (pp_buffer (PPI), F, S)
#define pp_wide_integer(PPI, I) \
   pp_format_scalar (PPI, HOST_WIDE_INT_PRINT_DEC, (HOST_WIDE_INT) I)
#define pp_pointer(PPI, P) pp_format_scalar (PPI, "%p", p)

#define pp_identifier(PPI, ID)  output_add_string (pp_buffer (PPI), ID)
#define pp_tree_identifier(PPI, T) pp_identifier(PPI, IDENTIFIER_POINTER (T))

#define pp_unsupported_tree(PPI, T) \
  output_verbatim (pp_buffer(PPI), "#`%s' not supported by %s#",\
                   tree_code_name[(int) TREE_CODE (T)], __FUNCTION__)

#endif /* GCC_PRETTY_PRINT_H */
