/* Define constants for communication with the CHILL parser.
   Copyright (C) 1992, 93, 94, 95, 96, 99, 2000 Free Software Foundation, Inc.

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
  RID_UNUSED,  /* keep this one first, please */
  RID_ALL,
  RID_ASSERTFAIL,
  RID_ASSOCIATION,
  RID_BIN,
  RID_BIT,
  RID_BOOL,
  RID_BOOLS,
  RID_BYTE,
  RID_CHAR,
  RID_CHARS,
  RID_DOUBLE,
  RID_DURATION,
  RID_DYNAMIC,
  RID_ELSE,
  RID_EMPTY,
  RID_FALSE,
  RID_FLOAT,
  RID_GENERAL,
  RID_IN,
  RID_INLINE,
  RID_INOUT,
  RID_INSTANCE,
  RID_INT,
  RID_LOC,
  RID_LONG,
  RID_LONG_REAL,
  RID_NULL,
  RID_OUT,
  RID_OVERFLOW,
  RID_PTR,
  RID_RANGE,
  RID_RANGEFAIL,
  RID_READ,
  RID_REAL,
  RID_RECURSIVE,
  RID_SHORT,
  RID_SIMPLE,
  RID_TIME,
  RID_TRUE,
  RID_UBYTE,
  RID_UINT,
  RID_ULONG,
  RID_UNSIGNED,
  RID_USHORT,
  RID_VOID,
  RID_MAX /* Last element */
};

#define NORID RID_UNUSED

#define RID_FIRST_MODIFIER RID_UNSIGNED

/* The elements of `ridpointers' are identifier nodes
   for the reserved type names and storage classes.
   It is indexed by a RID_... value.  */
extern tree ridpointers[(int) RID_MAX];

extern char *token_buffer;	/* Pointer to token buffer.  */

extern tree make_pointer_declarator PARAMS ((tree, tree));
extern void reinit_parse_for_function PARAMS ((void));
extern int yylex PARAMS ((void));

extern tree default_grant_file;
extern tree current_grant_file;

extern tree current_seize_file;

extern int chill_at_module_level;
extern tree chill_initializer_name;

extern void prepare_paren_colon PARAMS ((void));
