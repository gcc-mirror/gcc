/* GNU Objective C Runtime Miscellanious 
   Copyright (C) 1993 Free Software Foundation, Inc.

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
#include "error.h"

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
  write(2, msg, (size_t)strlen((char*)msg));
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

struct objc_ex_handler __ex_base_handler = {0, 0, 0, 0, 0 };
struct objc_ex_handler* __ex_last_handler = &__ex_base_handler;

objc_uncaught_exception_handler _objc_uncaught_exception_handler = 0;

void
__objc_add_handler(struct objc_ex_handler* hdlr)
{
  hdlr->ex_prev = __ex_last_handler;
  __ex_last_handler = hdlr;
}

void 
__objc_remove_handler(struct objc_ex_handler* hdlr)
{
  __ex_last_handler = hdlr->ex_prev;
}

#ifndef __STRICT_ANSI__
__volatile
#endif
extern void __objc_raise_error(int code, const void* data1, const void* data2)
{
  if (__ex_last_handler->ex_prev) 
    {
      __ex_last_handler->code = code;       
      __ex_last_handler->data1 = data1;     
      __ex_last_handler->data2 = data2;     
      longjmp (__ex_last_handler->ex_env, 1);
    } 
  else
    {
      if (_objc_uncaught_exception_handler)
	(*_objc_uncaught_exception_handler)(code, data1, data2);
      else
	{
	  printf ("uncaught exception: code=%d, data1=%x, data2=%x\n",
		  code, data1, data2);
	  abort();
	}
    }
}
