/* Basic error reporting routines.
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/* warning, error, and fatal.  These definitions are suitable for use
   in the generator programs; eventually we would like to use them in
   cc1 too, but that's a longer term project.  */

#ifndef __GCC_ERRORS_H__
#define __GCC_ERRORS_H__

void warning PVPROTO ((const char *format, ...)) ATTRIBUTE_PRINTF_1;
void error   PVPROTO ((const char *format, ...)) ATTRIBUTE_PRINTF_1;
void fatal   PVPROTO ((const char *format, ...))
    ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;

extern int have_error;
extern const char *progname;
    
#endif
