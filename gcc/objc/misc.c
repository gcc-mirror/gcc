/* GNU Objective C Runtime Miscellaneous 
   Copyright (C) 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
   Contrbuted by Kresten Krab Thorup

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#define __USE_FIXED_PROTOTYPES__
#include <stdlib.h>

#ifdef __alpha__
extern int write (int, const char*, int);
extern size_t strlen (const char*);
#endif

#include "runtime.h"

void objc_error(id object, const char* fmt, va_list);

void (*_objc_error)(id, const char*, va_list) = objc_error;

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

/*
** Standard functions for memory allocation and disposal.
** Users should use these functions in their ObjC programs so
** that they work properly with garbage collectors as well as
** can take advantage of the exception/error handling available.
*/

void *
objc_malloc(size_t size)
{
  void* res = (void*) (*_objc_malloc)(size);
  if(!res)
    objc_fatal("Virtual memory exhausted\n");
  return res;
}

void *
objc_atomic_malloc(size_t size)
{
  void* res = (void*) (*_objc_atomic_malloc)(size);
  if(!res)
    objc_fatal("Virtual memory exhausted\n");
  return res;
}

void *
objc_valloc(size_t size)
{
  void* res = (void*) (*_objc_valloc)(size);
  if(!res)
    objc_fatal("Virtual memory exhausted\n");
  return res;
}

void *
objc_realloc(void *mem, size_t size)
{
  void* res = (void*) (*_objc_realloc)(mem, size);
  if(!res)
    objc_fatal("Virtual memory exhausted\n");
  return res;
}

void *
objc_calloc(size_t nelem, size_t size)
{
  void* res = (void*) (*_objc_calloc)(nelem, size);
  if(!res)
    objc_fatal("Virtual memory exhausted\n");
  return res;
}

void
objc_free(void *mem)
{
  (*_objc_free)(mem);
}

/*
** Hook functions for memory allocation and disposal.
** This makes it easy to substitute garbage collection systems
** such as Boehm's GC by assigning these function pointers
** to the GC's allocation routines.  By default these point
** to the ANSI standard malloc, realloc, free, etc.
**
** Users should call the normal objc routines above for
** memory allocation and disposal within their programs.
*/
void *(*_objc_malloc)(size_t) = malloc;
void *(*_objc_atomic_malloc)(size_t) = malloc;
void *(*_objc_valloc)(size_t) = malloc;
void *(*_objc_realloc)(void *, size_t) = realloc;
void *(*_objc_calloc)(size_t, size_t) = calloc;
void (*_objc_free)(void *) = free;
