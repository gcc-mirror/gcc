/* Part of CPP library.  (Macro hash table support.)
   Copyright (C) 1997, 1998, 1999 Free Software Foundation, Inc.

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

#ifndef __GCC_CPPHASH__
#define __GCC_CPPHASH__

/* Structure allocated for every #define.  For a simple replacement
   such as
   	#define foo bar ,
   nargs = -1, the `pattern' list is null, and the expansion is just
   the replacement text.  Nargs = 0 means a functionlike macro with no args,
   e.g.,
       #define getchar() getc (stdin) .
   When there are args, the expansion is the replacement text with the
   args squashed out, and the reflist is a list describing how to
   build the output from the input: e.g., "3 chars, then the 1st arg,
   then 9 chars, then the 3rd arg, then 0 chars, then the 2nd arg".
   The chars here come from the expansion.  Whatever is left of the
   expansion after the last arg-occurrence is copied after that arg.
   Note that the reflist can be arbitrarily long---
   its length depends on the number of times the arguments appear in
   the replacement text, not how many args there are.  Example:
   #define f(x) x+x+x+x+x+x+x would have replacement text "++++++" and
   pattern list
     { (0, 1), (1, 1), (1, 1), ..., (1, 1), NULL }
   where (x, y) means (nchars, argno). */

struct reflist
{
  struct reflist *next;
  char stringify;		/* nonzero if this arg was preceded by a
				   # operator. */
  char raw_before;		/* Nonzero if a ## operator before arg. */
  char raw_after;		/* Nonzero if a ## operator after arg. */
  char rest_args;		/* Nonzero if this arg. absorbs the rest */
  int nchars;			/* Number of literal chars to copy before
				   this arg occurrence.  */
  int argno;			/* Number of arg to substitute (origin-0) */
};

typedef struct definition DEFINITION;
struct definition
{
  int nargs;
  int length;			/* length of expansion string */
  U_CHAR *expansion;
  int line;			/* Line number of definition */
  int col;
  const char *file;		/* File of definition */
  char rest_args;		/* Nonzero if last arg. absorbs the rest */
  struct reflist *pattern;

  /* Names of macro args, concatenated in order with commas between
     them.  The only use of this is that we warn on redefinition if
     this differs between the old and new definitions.  */
  U_CHAR *argnames;
};

/* different kinds of things that can appear in the value field
   of a hash node. */
union hashval
{
  const char *cpval;		/* some predefined macros */
  DEFINITION *defn;		/* #define */
  struct hashnode *aschain;	/* #assert */
};

typedef struct hashnode HASHNODE;
struct hashnode {
  struct hashnode *next;	/* double links for easy deletion */
  struct hashnode *prev;
  struct hashnode **bucket_hdr;	/* also, a back pointer to this node's hash
				   chain is kept, in case the node is the head
				   of the chain and gets deleted. */
  enum node_type type;		/* type of special token */
  int length;			/* length of token, for quick comparison */
  U_CHAR *name;			/* the actual name */
  union hashval value;		/* pointer to expansion, or whatever */
};

extern HASHNODE *_cpp_install	  PARAMS ((cpp_reader *, const U_CHAR *, int,
					   enum node_type, const char *));
extern HASHNODE *_cpp_lookup	  PARAMS ((cpp_reader *, const U_CHAR *, int));
extern void _cpp_free_definition  PARAMS ((DEFINITION *));
extern void _cpp_delete_macro	  PARAMS ((HASHNODE *));

extern DEFINITION *_cpp_create_definition
				  PARAMS ((cpp_reader *, int));
extern int _cpp_compare_defs		  PARAMS ((cpp_reader *, DEFINITION *,
					   DEFINITION *));
extern void _cpp_macroexpand	  PARAMS ((cpp_reader *, HASHNODE *));
extern void _cpp_dump_definition  PARAMS ((cpp_reader *, const U_CHAR *, long,
					   DEFINITION *));

#endif
