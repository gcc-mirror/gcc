// Support routines for the -*- C++ -*- dynamic memory management.
// Copyright (C) 1997, 1998 Free Software Foundation

// This file is part of GNU CC.

// GNU CC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// GNU CC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with GNU CC; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA. 

// As a special exception, if you link this library with other files,
// some of which are compiled with GCC, to produce an executable,
// this library does not by itself cause the resulting executable
// to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.

#include "new"
using std::new_handler;
using std::bad_alloc;

extern "C" void *malloc (size_t);
extern new_handler __new_handler;

#define WEAK(x) \
  x __attribute__ ((weak)); \
  x

#ifdef L_op_newnt
WEAK (void * operator new (size_t sz, const std::nothrow_t&) throw())
{
  void *p;

  /* malloc (0) is unpredictable; avoid it.  */
  if (sz == 0)
    sz = 1;
  p = (void *) malloc (sz);
  while (p == 0)
    {
      new_handler handler = __new_handler;
      if (! handler)
	return 0;
      try
	{
	  handler ();
	}
      catch (bad_alloc &)
	{
	  return 0;
	}

      p = (void *) malloc (sz);
    }

  return p;
}
#endif

#ifdef L_op_new
WEAK (void * operator new (size_t sz) throw (std::bad_alloc))
{
  void *p;

  /* malloc (0) is unpredictable; avoid it.  */
  if (sz == 0)
    sz = 1;
  p = (void *) malloc (sz);
  while (p == 0)
    {
      new_handler handler = __new_handler;
      if (! handler)
	throw bad_alloc ();
      handler ();
      p = (void *) malloc (sz);
    }

  return p;
}
#endif
