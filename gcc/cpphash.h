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

/* different kinds of things that can appear in the value field
   of a hash node. */
union hashval
{
  const char *cpval;		/* some predefined macros */
  DEFINITION *defn;		/* #define */
  struct hashnode *aschain;	/* #assert */
};

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

typedef struct hashnode HASHNODE;

/* Some definitions for the hash table.  The hash function MUST be
   computed as shown in hashf () below.  That is because the rescan
   loop computes the hash value `on the fly' for most tokens,
   in order to avoid the overhead of a lot of procedure calls to
   the hashf () function.  Hashf () only exists for the sake of
   politeness, for use when speed isn't so important. */

#define HASHSTEP(old, c) ((old << 2) + c)
#define MAKE_POS(v) (v & 0x7fffffff) /* make number positive */

extern HASHNODE *cpp_install	  PARAMS ((cpp_reader *, U_CHAR *, int,
					   enum node_type, const char *, int));
extern int hashf		  PARAMS ((const U_CHAR *, int, int));
extern void delete_macro	  PARAMS ((HASHNODE *));

extern MACRODEF create_definition PARAMS ((U_CHAR *, U_CHAR *,
					   cpp_reader *, int));
extern int compare_defs		  PARAMS ((cpp_reader *, DEFINITION *,
					   DEFINITION *));
extern void macroexpand		  PARAMS ((cpp_reader *, HASHNODE *));
extern void dump_definition	  PARAMS ((cpp_reader *, MACRODEF));
