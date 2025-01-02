/* d-frontend.h -- D frontend interface to the gcc back-end.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.

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

#ifndef GCC_D_FRONTEND_H
#define GCC_D_FRONTEND_H

/* These functions are defined in D runtime.  */
extern "C" int rt_init (void);
extern "C" int rt_term (void);
extern "C" void gc_disable (void);
extern "C" void *gc_malloc (size_t sz, unsigned ba = 0, const void *ti = NULL);
extern "C" void gc_free (void *);
extern "C" void gc_collect (void);

template<typename T>
inline T *
d_gc_malloc (void)
{
  void *ptr = gc_malloc (sizeof (T));
  return new(ptr) T ();
}

#endif
