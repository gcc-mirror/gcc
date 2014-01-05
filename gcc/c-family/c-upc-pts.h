/* Define UPC pointer-to-shared representation characteristics.
   Copyright (C) 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

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

#ifndef GCC_C_FAMILY_UPC_PTS_H
#define GCC_C_FAMILY_UPC_PTS_H 1

#ifdef HAVE_UPC_PTS_PACKED_REP
  /* 'packed' UPC pointer-to-shared representation */
  #define UPC_PTS_SIZE 64
#else
  /* 'struct' UPC pointer-to-shared representation */
  #define UPC_PTS_SIZE            (LONG_TYPE_SIZE + POINTER_SIZE)
  #define UPC_PTS_PHASE_SIZE      (LONG_TYPE_SIZE/2)
  #define UPC_PTS_THREAD_SIZE     (LONG_TYPE_SIZE/2)
  #define UPC_PTS_VADDR_SIZE      POINTER_SIZE
  #define UPC_PTS_PHASE_TYPE      ((LONG_TYPE_SIZE == 64) ? "uint32_t" : "uint16_t")
  #define UPC_PTS_THREAD_TYPE     ((LONG_TYPE_SIZE == 64) ? "uint32_t" : "uint16_t")
  #define UPC_PTS_VADDR_TYPE      "char *"
#endif /* HAVE_UPC_PTS_PACKED_REP */

#ifdef HAVE_UPC_PTS_VADDR_FIRST
  #define UPC_PTS_PHASE_SHIFT     0
  #define UPC_PTS_THREAD_SHIFT    UPC_PTS_PHASE_SIZE
  #define UPC_PTS_VADDR_SHIFT     (UPC_PTS_PHASE_SIZE+UPC_PTS_THREAD_SIZE)
#else
  #define UPC_PTS_PHASE_SHIFT     (UPC_PTS_VADDR_SIZE+UPC_PTS_THREAD_SIZE)
  #define UPC_PTS_THREAD_SHIFT    UPC_PTS_VADDR_SIZE
  #define UPC_PTS_VADDR_SHIFT     0
#endif
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
  #define UPC_PTS_VADDR_MASK ((1L << UPC_PTS_VADDR_SIZE) - 1L)
#elif HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONGLONG
  #define UPC_PTS_VADDR_MASK ((1LL << UPC_PTS_VADDR_SIZE) - 1LL)
#else
  #error Unexpected "HOST_BITS_PER_WIDE_INT" value.
#endif /* HOST_BITS_PER_WIDE_INT */
#define UPC_PTS_THREAD_MASK ((1 << UPC_PTS_THREAD_SIZE) - 1)
#define UPC_PTS_PHASE_MASK ((1 << UPC_PTS_PHASE_SIZE) - 1)

#ifdef HAVE_UPC_PTS_VADDR_FIRST
  #define UPC_PTS_VADDR_FIRST 1
#else
  #define UPC_PTS_VADDR_FIRST 0
#endif /* HAVE_UPC_PTS_VADDR_FIRST */

#define UPC_MAX_THREADS (1 << (((UPC_PTS_THREAD_SIZE) < 30) \
				 ? (UPC_PTS_THREAD_SIZE) : 30))
#define UPC_MAX_BLOCK_SIZE ((1 << (((UPC_PTS_PHASE_SIZE) < 31) \
				    ? (UPC_PTS_PHASE_SIZE) : 31)) - 1)

#endif  /* !GCC_C_FAMILY_UPC_PTS_H */
