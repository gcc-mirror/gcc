/* Interface for the GNU C++ pretty-printer.
   Copyright (C) 2003 Free Software Foundation, Inc.
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

#ifndef GCC_CXX_PRETTY_PRINT_H
#define GCC_CXX_PRETTY_PRINT_H

#include "c-pretty-print.h"

#undef pp_c_base
#define pp_c_base(PP) (&(PP)->c_base)

typedef enum
{
  /* Ask for an qualified-id.  */
  pp_cxx_flag_default_argument = 1 << pp_c_flag_last_bit
  
} cxx_pretty_printer_flags;

typedef struct
{
  c_pretty_printer c_base;
  /* This is the enclosing scope of the entity being pretty-printed.  */
  tree enclosing_scope;
} cxx_pretty_printer;

void pp_cxx_pretty_printer_init (cxx_pretty_printer *);

void pp_cxx_declaration (cxx_pretty_printer *, tree);
void pp_cxx_function_definition (cxx_pretty_printer *, tree);
void pp_cxx_canonical_template_parameter (cxx_pretty_printer *, tree);
void pp_cxx_statement (cxx_pretty_printer *, tree);


#endif /* GCC_CXX_PRETTY_PRINT_H */
