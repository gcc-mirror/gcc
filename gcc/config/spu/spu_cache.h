/* Copyright (C) 2008-2017 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _SPU_CACHE_H
#define _SPU_CACHE_H

void *__cache_fetch_dirty (__ea void *ea, int n_bytes_dirty);
void *__cache_fetch (__ea void *ea);
void __cache_evict (__ea void *ea);
void __cache_flush (void);
void __cache_touch (__ea void *ea);

#define cache_fetch_dirty(_ea, _n_bytes_dirty) \
     __cache_fetch_dirty(_ea, _n_bytes_dirty)

#define cache_fetch(_ea) __cache_fetch(_ea)
#define cache_touch(_ea) __cache_touch(_ea)
#define cache_evict(_ea) __cache_evict(_ea)
#define cache_flush() __cache_flush()

#endif
