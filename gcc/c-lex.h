/* Define constants for communication with c-parse.y.
   Copyright (C) 1987, 1992 Free Software Foundation, Inc.

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



enum rid
{
  RID_UNUSED,
  RID_INT,
  RID_CHAR,
  RID_FLOAT,
  RID_DOUBLE,
  RID_VOID,
  RID_UNUSED1,

  RID_UNSIGNED,
  RID_SHORT,
  RID_LONG,
  RID_AUTO,
  RID_STATIC,
  RID_EXTERN,
  RID_REGISTER,
  RID_TYPEDEF,
  RID_SIGNED,
  RID_CONST,
  RID_VOLATILE,
  RID_INLINE,
  RID_NOALIAS,
  RID_ITERATOR,
  RID_COMPLEX,

  RID_IN,
  RID_OUT,
  RID_INOUT,
  RID_BYCOPY,
  RID_ONEWAY,
  RID_ID,

  RID_MAX
};

#define NORID RID_UNUSED

#define RID_FIRST_MODIFIER RID_UNSIGNED

/* The elements of `ridpointers' are identifier nodes
   for the reserved type names and storage classes.
   It is indexed by a RID_... value.  */
extern tree ridpointers[(int) RID_MAX];

/* the declaration found for the last IDENTIFIER token read in.
   yylex must look this up to detect typedefs, which get token type TYPENAME,
   so it is left around in case the identifier is not a typedef but is
   used in a context which makes it a reference to a variable.  */
extern tree lastiddecl;

extern char *token_buffer;	/* Pointer to token buffer.  */

extern tree make_pointer_declarator ();
extern void reinit_parse_for_function ();
extern int yylex ();

extern char *get_directive_line ();
