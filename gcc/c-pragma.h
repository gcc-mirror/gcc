/* Pragma related interfaces.
   Copyright (C) 1995, 1998, 1999 Free Software Foundation, Inc.

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

#ifndef _C_PRAGMA_H
#define _C_PRAGMA_H

#ifdef HANDLE_SYSV_PRAGMA
/* Support #pragma weak iff ASM_WEAKEN_LABEL and ASM_OUTPUT_DEF are
   defined.  */
#if defined (ASM_WEAKEN_LABEL) && defined (ASM_OUTPUT_DEF)
#define HANDLE_PRAGMA_WEAK SUPPORTS_WEAK
#endif

/* We always support #pragma pack for SYSV pragmas.  */
#ifndef HANDLE_PRAGMA_PACK
#define HANDLE_PRAGMA_PACK 1
#endif
#endif /* HANDLE_SYSV_PRAGMA */


#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
/* If we are supporting #pragma pack(push... then we automatically
   support #pragma pack(<n>)  */
#define HANDLE_PRAGMA_PACK 1
#define PRAGMA_INSERT_ATTRIBUTES(node, pattr, prefix_attr) \
  insert_pack_attributes (node, pattr, prefix_attr)
extern void insert_pack_attributes PROTO((tree, tree *, tree *));
#endif /* HANDLE_PRAGMA_PACK_PUSH_POP */


#ifdef HANDLE_PRAGMA_WEAK
/* This structure contains any weak symbol declarations waiting to be emitted.  */
struct weak_syms
{
  struct weak_syms * next;
  char * name;
  char * value;
};

/* Declared in varasm.c */
extern struct weak_syms * weak_decls;

extern int add_weak PROTO((char *, char *));
#endif /* HANDLE_PRAGMA_WEAK */


#if defined HANDLE_PRAGMA_PACK || defined HANDLE_PRAGMA_WEAK
/* Define HANDLE_GENERIC_PRAGMAS if any kind of front-end pragma
   parsing is to be done.  The code in GCC's generic C source files
   will only look for the definition of this constant.  They will
   ignore definitions of HANDLE_PRAGMA_PACK and so on.  */
#define HANDLE_GENERIC_PRAGMAS 1
#endif


#ifdef HANDLE_GENERIC_PRAGMAS
enum pragma_state
{
  ps_start,
  ps_done,
#ifdef HANDLE_PRAGMA_WEAK
  ps_weak,
  ps_name,
  ps_equals,
  ps_value,
#endif
#ifdef HANDLE_PRAGMA_PACK
  ps_pack,
  ps_left,
  ps_align,
  ps_right,
#endif
#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
  ps_push, ps_pushcomma, ps_pushid, ps_pushcomma2,
  ps_pop, ps_popcomma,
#endif
  ps_bad
};

/* Handle a C style pragma */
extern int handle_pragma_token PROTO((const char *, tree));

#endif /* HANDLE_GENERIC_PRAGMAS */
#endif /* _C_PRAGMA_H */
