/* Header file for collect/tlink routines.  
   Copyright (C) 1998 Free Software Foundation, Inc.

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

#ifndef GCC_COLLECT2_H
#define GCC_COLLECT2_H

extern void do_tlink PARAMS ((char **, char **));

extern void collect_execute PARAMS ((const char *, char **, const char *));

extern void collect_exit PARAMS ((int)) ATTRIBUTE_NORETURN;

extern int collect_wait PARAMS ((const char *));

extern void dump_file PARAMS ((const char *));

extern int file_exists PARAMS ((const char *));

extern const char *ldout;
extern const char *c_file_name;
extern struct obstack temporary_obstack;
extern struct obstack permanent_obstack;
extern char *temporary_firstobj;
extern int vflag, debug;

extern void fancy_abort PARAMS ((void)) ATTRIBUTE_NORETURN;
extern void error PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;
extern void notice PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;
extern void fatal PARAMS ((const char *, ...)) 
  ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
extern void fatal_perror PARAMS ((const char *, ...))
  ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;

#endif /* ! GCC_COLLECT2_H */
