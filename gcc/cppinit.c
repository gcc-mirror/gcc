/* CPP Library.
   Copyright (C) 1986, 87, 89, 92-98, 1999 Free Software Foundation, Inc.
   Contributed by Per Bothner, 1994-95.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* This file will have more stuff in it eventually, but right now
   we just have one hack: we move all the is_* table initialization
   in here, and we can declare them const in cpplib.h, which improves
   code a bit. */

#include "config.h"
#include "system.h"

typedef unsigned char U_CHAR;

/* table to tell if char can be part of a C identifier.  */
U_CHAR is_idchar[256] = { 0 };
/* table to tell if char can be first char of a c identifier.  */
U_CHAR is_idstart[256] = { 0 };
/* table to tell if c is horizontal space.  */
U_CHAR is_hor_space[256] = { 0 };
/* table to tell if c is horizontal or vertical space.  */
U_CHAR is_space[256] = { 0 };
/* Table to handle trigraph conversion, which occurs before all other
   processing, everywhere in the file.  (This is necessary since one
   of the trigraphs encodes backslash.)  Note it's off by default.

	from	to	from	to	from	to
	?? =	#	?? )	]	?? !	|
	?? (	[	?? '	^	?? >	}
	?? /	\	?? <	{	?? -	~

   There is not a space between the ?? and the third char.  I put spaces
   there to avoid warnings when compiling this file. */
U_CHAR trigraph_table[256] = { 0 };

/* Initialize syntactic classifications of characters. */
void
initialize_char_syntax (dollar_in_ident)
     int dollar_in_ident;
{
  is_idstart['a'] = 1; is_idstart['b'] = 1; is_idstart['c'] = 1;
  is_idstart['d'] = 1; is_idstart['e'] = 1; is_idstart['f'] = 1;
  is_idstart['g'] = 1; is_idstart['h'] = 1; is_idstart['i'] = 1;
  is_idstart['j'] = 1; is_idstart['k'] = 1; is_idstart['l'] = 1;
  is_idstart['m'] = 1; is_idstart['n'] = 1; is_idstart['o'] = 1;
  is_idstart['p'] = 1; is_idstart['q'] = 1; is_idstart['r'] = 1;
  is_idstart['s'] = 1; is_idstart['t'] = 1; is_idstart['u'] = 1;
  is_idstart['v'] = 1; is_idstart['w'] = 1; is_idstart['x'] = 1;
  is_idstart['y'] = 1; is_idstart['z'] = 1;

  is_idstart['A'] = 1; is_idstart['B'] = 1; is_idstart['C'] = 1;
  is_idstart['D'] = 1; is_idstart['E'] = 1; is_idstart['F'] = 1;
  is_idstart['G'] = 1; is_idstart['H'] = 1; is_idstart['I'] = 1;
  is_idstart['J'] = 1; is_idstart['K'] = 1; is_idstart['L'] = 1;
  is_idstart['M'] = 1; is_idstart['N'] = 1; is_idstart['O'] = 1;
  is_idstart['P'] = 1; is_idstart['Q'] = 1; is_idstart['R'] = 1;
  is_idstart['S'] = 1; is_idstart['T'] = 1; is_idstart['U'] = 1;
  is_idstart['V'] = 1; is_idstart['W'] = 1; is_idstart['X'] = 1;
  is_idstart['Y'] = 1; is_idstart['Z'] = 1;

  is_idstart['_'] = 1;

  is_idchar['a'] = 1; is_idchar['b'] = 1; is_idchar['c'] = 1;
  is_idchar['d'] = 1; is_idchar['e'] = 1; is_idchar['f'] = 1;
  is_idchar['g'] = 1; is_idchar['h'] = 1; is_idchar['i'] = 1;
  is_idchar['j'] = 1; is_idchar['k'] = 1; is_idchar['l'] = 1;
  is_idchar['m'] = 1; is_idchar['n'] = 1; is_idchar['o'] = 1;
  is_idchar['p'] = 1;  is_idchar['q'] = 1; is_idchar['r'] = 1;
  is_idchar['s'] = 1; is_idchar['t'] = 1;  is_idchar['u'] = 1;
  is_idchar['v'] = 1; is_idchar['w'] = 1; is_idchar['x'] = 1;
  is_idchar['y'] = 1; is_idchar['z'] = 1;

  is_idchar['A'] = 1; is_idchar['B'] = 1; is_idchar['C'] = 1;
  is_idchar['D'] = 1; is_idchar['E'] = 1; is_idchar['F'] = 1;
  is_idchar['G'] = 1; is_idchar['H'] = 1; is_idchar['I'] = 1;
  is_idchar['J'] = 1; is_idchar['K'] = 1; is_idchar['L'] = 1;
  is_idchar['M'] = 1; is_idchar['N'] = 1; is_idchar['O'] = 1;
  is_idchar['P'] = 1; is_idchar['Q'] = 1; is_idchar['R'] = 1;
  is_idchar['S'] = 1; is_idchar['T'] = 1;  is_idchar['U'] = 1;
  is_idchar['V'] = 1; is_idchar['W'] = 1; is_idchar['X'] = 1;
  is_idchar['Y'] = 1; is_idchar['Z'] = 1;

  is_idchar['1'] = 1; is_idchar['2'] = 1; is_idchar['3'] = 1;
  is_idchar['4'] = 1; is_idchar['5'] = 1; is_idchar['6'] = 1;
  is_idchar['7'] = 1; is_idchar['8'] = 1; is_idchar['9'] = 1;
  is_idchar['0'] = 1;

  is_idchar['_']  = 1;

  /* These will be reset later if -$ is in effect. */
  is_idchar['$']  = dollar_in_ident;
  is_idstart['$'] = dollar_in_ident;

  /* horizontal space table */
  is_hor_space[' '] = 1;
  is_hor_space['\t'] = 1;
  is_hor_space['\v'] = 1;
  is_hor_space['\f'] = 1;
  is_hor_space['\r'] = 1;

  is_space[' '] = 1;
  is_space['\t'] = 1;
  is_space['\v'] = 1;
  is_space['\f'] = 1;
  is_space['\n'] = 1;
  is_space['\r'] = 1;

  /* trigraph conversion */
  trigraph_table['='] = '#';  trigraph_table[')'] = ']';
  trigraph_table['!'] = '|';  trigraph_table['('] = '[';
  trigraph_table['\''] = '^'; trigraph_table['>'] = '}';
  trigraph_table['/'] = '\\'; trigraph_table['<'] = '{';
  trigraph_table['-'] = '~';
}
