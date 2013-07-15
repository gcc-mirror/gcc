/* Hardware Transactional Memory (HTM) intrinsics.
   Copyright (C) 2013 Free Software Foundation, Inc.
   Contributed by Peter Bergner <bergner@vnet.ibm.com>.

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

#ifndef __HTM__
# error "HTM instruction set not enabled"
#endif /* __HTM__ */

#ifndef _HTMINTRIN_H
#define _HTMINTRIN_H

#include <stdint.h>

typedef uint64_t texasr_t;
typedef uint32_t texasru_t;
typedef uint32_t texasrl_t;
typedef uintptr_t tfiar_t;
typedef uintptr_t tfhar_t;

#define _HTM_STATE(CR0) ((CR0 >> 1) & 0x3)
#define _HTM_NONTRANSACTIONAL 0x0
#define _HTM_SUSPENDED        0x1
#define _HTM_TRANSACTIONAL    0x2

/* The following macros use the IBM bit numbering for BITNUM
   as used in the ISA documentation.  */

#define _TEXASR_EXTRACT_BITS(TEXASR,BITNUM,SIZE) \
  (((TEXASR) >> (63-(BITNUM))) & ((1<<(SIZE))-1))
#define _TEXASRU_EXTRACT_BITS(TEXASR,BITNUM,SIZE) \
  (((TEXASR) >> (31-(BITNUM))) & ((1<<(SIZE))-1))

#define _TEXASR_FAILURE_CODE(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 7, 8)
#define _TEXASRU_FAILURE_CODE(TEXASRU) \
  _TEXASRU_EXTRACT_BITS(TEXASRU, 7, 8)

#define _TEXASR_FAILURE_PERSISTENT(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 7, 1)
#define _TEXASRU_FAILURE_PERSISTENT(TEXASRU) \
  _TEXASRU_EXTRACT_BITS(TEXASRU, 7, 1)

#define _TEXASR_DISALLOWED(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 8, 1)
#define _TEXASRU_DISALLOWED(TEXASRU) \
  _TEXASRU_EXTRACT_BITS(TEXASRU, 8, 1)

#define _TEXASR_NESTING_OVERFLOW(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 9, 1)
#define _TEXASRU_NESTING_OVERFLOW(TEXASRU) \
  _TEXASRU_EXTRACT_BITS(TEXASRU, 9, 1)

#define _TEXASR_FOOTPRINT_OVERFLOW(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 10, 1)
#define _TEXASRU_FOOTPRINT_OVERFLOW(TEXASRU) \
  _TEXASRU_EXTRACT_BITS(TEXASRU, 10, 1)

#define _TEXASR_SELF_INDUCED_CONFLICT(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 11, 1)
#define _TEXASRU_SELF_INDUCED_CONFLICT(TEXASRU) \
  _TEXASRU_EXTRACT_BITS(TEXASRU, 11, 1)

#define _TEXASR_NON_TRANSACTIONAL_CONFLICT(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 12, 1)
#define _TEXASRU_NON_TRANSACTIONAL_CONFLICT(TEXASRU) \
  _TEXASRU_EXTRACT_BITS(TEXASRU, 12, 1)

#define _TEXASR_TRANSACTION_CONFLICT(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 13, 1)
#define _TEXASRU_TRANSACTION_CONFLICT(TEXASRU) \
  _TEXASRU_EXTRACT_BITS(TEXASRU, 13, 1)

#define _TEXASR_TRANSLATION_INVALIDATION_CONFLICT(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 14, 1)
#define _TEXASRU_TRANSLATION_INVALIDATION_CONFLICT(TEXASRU) \
  _TEXASRU_EXTRACT_BITS(TEXASRU, 14, 1)

#define _TEXASR_IMPLEMENTAION_SPECIFIC(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 15, 1)
#define _TEXASRU_IMPLEMENTAION_SPECIFIC(TEXASRU) \
  _TEXASRU_EXTRACT_BITS(TEXASRU, 15, 1)

#define _TEXASR_INSRUCTION_FETCH_CONFLICT(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 16, 1)
#define _TEXASRU_INSRUCTION_FETCH_CONFLICT(TEXASRU) \
  _TEXASRU_EXTRACT_BITS(TEXASRU, 16, 1)

#define _TEXASR_ABORT(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 31, 1)
#define _TEXASRU_ABORT(TEXASRU) \
  _TEXASRU_EXTRACT_BITS(TEXASRU, 31, 1)


#define _TEXASR_SUSPENDED(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 32, 1)

#define _TEXASR_PRIVILEGE(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 35, 2)

#define _TEXASR_FAILURE_SUMMARY(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 36, 1)

#define _TEXASR_TFIAR_EXACT(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 37, 1)

#define _TEXASR_ROT(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 38, 1)

#define _TEXASR_TRANSACTION_LEVEL(TEXASR) \
  _TEXASR_EXTRACT_BITS(TEXASR, 63, 12)

#endif /* _HTMINTRIN_H */
