/* Subroutines for the C front end on the POWER and PowerPC architectures.
   Copyright (C) 2002
   Free Software Foundation, Inc.

   Contributed by Zack Weinberg <zack@codesourcery.com>

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

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "tree.h"
#include "c-lex.h"
#include "errors.h"
#include "tm_p.h"

/* Handle the machine specific pragma longcall.  Its syntax is

   # pragma longcall ( TOGGLE )

   where TOGGLE is either 0 or 1.

   rs6000_default_long_calls is set to the value of TOGGLE, changing
   whether or not new function declarations receive a longcall
   attribute by default.  */

#define SYNTAX_ERROR(msgid) do {					\
  warning (msgid);					\
  warning ("ignoring malformed #pragma longcall");	\
  return;						\
} while (0)

void
rs6000_pragma_longcall (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  tree x, n;

  /* If we get here, generic code has already scanned the directive
     leader and the word "longcall".  */

  if (c_lex (&x) != CPP_OPEN_PAREN)
    SYNTAX_ERROR ("missing open paren");
  if (c_lex (&n) != CPP_NUMBER)
    SYNTAX_ERROR ("missing number");
  if (c_lex (&x) != CPP_CLOSE_PAREN)
    SYNTAX_ERROR ("missing close paren");

  if (n != integer_zero_node && n != integer_one_node)
    SYNTAX_ERROR ("number must be 0 or 1");

  if (c_lex (&x) != CPP_EOF)
    warning ("junk at end of #pragma longcall");

  rs6000_default_long_calls = (n == integer_one_node);
}
