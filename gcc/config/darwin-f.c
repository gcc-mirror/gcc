/* Darwin support needed only by Fortran frontends.
   Copyright (C) 2008-2016 Free Software Foundation, Inc.
   Contributed by Daniel Franke.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


/* Provide stubs for the hooks defined by darwin.h
     TARGET_EXTRA_PRE_INCLUDES, TARGET_EXTRA_INCLUDES

   As both, gcc and gfortran link in incpath.o, we can not
   conditionally undefine said hooks if fortran is build.
   However, we can define do-nothing stubs of said hooks as
   we are not interested in objc include files in Fortran.

   The hooks original purpose (see also darwin-c.c):
    * darwin_register_objc_includes
      Register the GNU objective-C runtime include path if STDINC.

    * darwin_register_frameworks
      Register all the system framework paths if STDINC is true and setup
      the missing_header callback for subframework searching if any
      frameworks had been registered.  */


#include "ansidecl.h"

/* Prototypes for functions below to avoid a lengthy list of includes
   to achieve the same.  */
void darwin_register_objc_includes (const char *, const char *, int);
void darwin_register_frameworks (const char *, const char *, int);


void
darwin_register_objc_includes (const char *sysroot ATTRIBUTE_UNUSED,
			       const char *iprefix ATTRIBUTE_UNUSED,
			       int stdinc ATTRIBUTE_UNUSED)
{
}

void
darwin_register_frameworks (const char *sysroot ATTRIBUTE_UNUSED,
			    const char *iprefix ATTRIBUTE_UNUSED,
			    int stdinc ATTRIBUTE_UNUSED)
{
}
