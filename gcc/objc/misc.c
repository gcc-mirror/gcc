/* GNU Objective C Runtime Miscellanious 
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

Author: Kresten Krab Thorup

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2, or (at your option) any later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

You should have received a copy of the GNU General Public License along with
   GNU CC; see the file COPYING.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#include "runtime.h"

void objc_error(id object, const char* fmt, va_list);

void (*_objc_error)(id, const char*, va_list) = objc_error;

#ifdef __alpha__
#include <stdlib.h>
extern int write (int, const char*, int);
extern size_t strlen (const char*);
#endif

void
objc_error(id object, const char* fmt, va_list ap)
{
  vfprintf (stderr, fmt, ap);
  abort ();
}

volatile void
objc_fatal(const char* msg)
{
  write(2, msg, (int)strlen((const char*)msg));
  abort();
}

void*
__objc_xmalloc(size_t size)
{
  void* res = (void*) malloc(size);
  if(!res)
    objc_fatal("Virtual memory exhausted\n");
  return res;
}

void*
__objc_xrealloc(void* mem, size_t size)
{
  void* res = (void*) realloc(mem, size);
  if(!res)
    objc_fatal("Virtual memory exhausted\n");
  return res;
}

void*
__objc_xcalloc(size_t nelem, size_t size)
{
  void* res = (void*)calloc(nelem, size);
  if(!res)
    objc_fatal("Virtual memory exhausted\n");
  return res;
}
