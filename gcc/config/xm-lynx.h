/* Configuration for GNU C-compiler for Lynx.
   Copyright (C) 1993, 1995 Free Software Foundation, Inc.

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

/* This file defines machine-independent things specific to a host
   running Lynx.  This file should not be specified as $xm_file itself;
   instead $xm_file should be CPU/xm-lynx.h, which should include this one.  */

/* #defines that need visibility everywhere.  */
#define FALSE 0
#define TRUE 1

/* Arguments to use with `exit'.  */
#define SUCCESS_EXIT_CODE 0
#define FATAL_EXIT_CODE 33

/* Lynx has no vfork system call.  */
#define vfork fork

/* Lynx has a non-standard mktemp function.  */
/* ??? This is simpler than creating YATM: Yet Another Target Macro.  */
#define mktemp lynx_mktemp

#define lynx_mktemp(template) \
do {				\
  int pid = getpid ();		\
  char *t = template;		\
  char *p;			\
  p = t + strlen (t);		\
  while (*--p == 'X')		\
    {				\
      *p = (pid % 10) + '0';	\
      pid /= 10;		\
    }				\
} while (0)
