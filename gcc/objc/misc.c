/* GNU Objective C Runtime Miscellaneous 
   Copyright (C) 1993, 1994, 1995, 1996, 1997 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup

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
#include "runtime.h"

/*
** Error handler function
** NULL so that default is to just print to stderr
*/
static objc_error_handler _objc_error_handler = NULL;

/* Trigger an objc error */
void
objc_error(id object, int code, const char* fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  objc_verror(object, code, fmt, ap);
  va_end(ap);
}

/* Trigger an objc error */
void
objc_verror(id object, int code, const char* fmt, va_list ap)
{
  BOOL result = NO;

  /* Call the error handler if its there
     Otherwise print to stderr */
  if (_objc_error_handler)
    result = (*_objc_error_handler)(object, code, fmt, ap);
  else
    vfprintf (stderr, fmt, ap);

  /* Continue if the error handler says its ok
     Otherwise abort the program */
  if (result)
    return;
  else
    abort();
}

/* Set the error handler */
objc_error_handler
objc_set_error_handler(objc_error_handler func)
{
  objc_error_handler temp = _objc_error_handler;
  _objc_error_handler = func;
  return temp;
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
    objc_error(nil, OBJC_ERR_MEMORY, "Virtual memory exhausted\n");
  return res;
}

void *
objc_atomic_malloc(size_t size)
{
  void* res = (void*) (*_objc_atomic_malloc)(size);
  if(!res)
    objc_error(nil, OBJC_ERR_MEMORY, "Virtual memory exhausted\n");
  return res;
}

void *
objc_valloc(size_t size)
{
  void* res = (void*) (*_objc_valloc)(size);
  if(!res)
    objc_error(nil, OBJC_ERR_MEMORY, "Virtual memory exhausted\n");
  return res;
}

void *
objc_realloc(void *mem, size_t size)
{
  void* res = (void*) (*_objc_realloc)(mem, size);
  if(!res)
    objc_error(nil, OBJC_ERR_MEMORY, "Virtual memory exhausted\n");
  return res;
}

void *
objc_calloc(size_t nelem, size_t size)
{
  void* res = (void*) (*_objc_calloc)(nelem, size);
  if(!res)
    objc_error(nil, OBJC_ERR_MEMORY, "Virtual memory exhausted\n");
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
