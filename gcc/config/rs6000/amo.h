/* Power ISA 3.0 atomic memory operation include file.
   Copyright (C) 2017 Free Software Foundation, Inc.
   Contributed by Michael Meissner <meissner@linux.vnet.ibm.com>.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _AMO_H
#define _AMO_H

#if !defined(_ARCH_PWR9) || !defined(_ARCH_PPC64)
#error "The atomic memory operations require Power 64-bit ISA 3.0"

#else
#include <stdint.h>

/* Enumeration of the LWAT/LDAT sub-opcodes.  */
enum _AMO_LD {
  _AMO_LD_ADD		= 0x00,		/* Fetch and Add.  */
  _AMO_LD_XOR		= 0x01,		/* Fetch and Xor.  */
  _AMO_LD_IOR		= 0x02,		/* Fetch and Ior.  */
  _AMO_LD_AND		= 0x03,		/* Fetch and And.  */
  _AMO_LD_UMAX		= 0x04,		/* Fetch and Unsigned Maximum.  */
  _AMO_LD_SMAX		= 0x05,		/* Fetch and Signed Maximum.  */
  _AMO_LD_UMIN		= 0x06,		/* Fetch and Unsigned Minimum.  */
  _AMO_LD_SMIN		= 0x07,		/* Fetch and Signed Minimum.  */
  _AMO_LD_SWAP		= 0x08,		/* Swap.  */
  _AMO_LD_CS_NE		= 0x10,		/* Compare and Swap Not Equal.  */
  _AMO_LD_INC_BOUNDED	= 0x18,		/* Fetch and Increment Bounded.  */
  _AMO_LD_INC_EQUAL	= 0x19,		/* Fetch and Increment Equal.  */
  _AMO_LD_DEC_BOUNDED	= 0x1A		/* Fetch and Decrement Bounded.  */
};

/* Implementation of the simple LWAT/LDAT operations that take one register and
   modify one word or double-word of memory and return the value that was
   previously in the memory location.

   The LWAT/LDAT opcode requires the address to be a single register, and that
   points to a suitably aligned memory location.  Asm volatile is used to
   prevent the optimizer from moving the operation.  */

#define _AMO_LD_SIMPLE(NAME, TYPE, OPCODE, FC)				\
static __inline__ TYPE							\
NAME (TYPE *_PTR, TYPE _VALUE)						\
{									\
  unsigned __int128 _TMP;						\
  TYPE _RET;								\
  __asm__ volatile ("mr %L1,%3\n"					\
		    "\t" OPCODE " %1,%P0,%4\n"				\
		    "\tmr %2,%1\n"					\
		    : "+Q" (_PTR[0]), "=&r" (_TMP), "=r" (_RET)		\
		    : "r" (_VALUE), "n" (FC));				\
  return _RET;								\
}

_AMO_LD_SIMPLE (amo_lwat_add,   uint32_t, "lwat", _AMO_LD_ADD)
_AMO_LD_SIMPLE (amo_lwat_xor,   uint32_t, "lwat", _AMO_LD_XOR)
_AMO_LD_SIMPLE (amo_lwat_ior,   uint32_t, "lwat", _AMO_LD_IOR)
_AMO_LD_SIMPLE (amo_lwat_and,   uint32_t, "lwat", _AMO_LD_AND)
_AMO_LD_SIMPLE (amo_lwat_umax,  uint32_t, "lwat", _AMO_LD_UMAX)
_AMO_LD_SIMPLE (amo_lwat_umin,  uint32_t, "lwat", _AMO_LD_UMIN)
_AMO_LD_SIMPLE (amo_lwat_swap,  uint32_t, "lwat", _AMO_LD_SWAP)

_AMO_LD_SIMPLE (amo_lwat_sadd,  int32_t,  "lwat", _AMO_LD_ADD)
_AMO_LD_SIMPLE (amo_lwat_smax,  int32_t,  "lwat", _AMO_LD_SMAX)
_AMO_LD_SIMPLE (amo_lwat_smin,  int32_t,  "lwat", _AMO_LD_SMIN)
_AMO_LD_SIMPLE (amo_lwat_sswap, int32_t,  "lwat", _AMO_LD_SWAP)

_AMO_LD_SIMPLE (amo_ldat_add,   uint64_t, "ldat", _AMO_LD_ADD)
_AMO_LD_SIMPLE (amo_ldat_xor,   uint64_t, "ldat", _AMO_LD_XOR)
_AMO_LD_SIMPLE (amo_ldat_ior,   uint64_t, "ldat", _AMO_LD_IOR)
_AMO_LD_SIMPLE (amo_ldat_and,   uint64_t, "ldat", _AMO_LD_AND)
_AMO_LD_SIMPLE (amo_ldat_umax,  uint64_t, "ldat", _AMO_LD_UMAX)
_AMO_LD_SIMPLE (amo_ldat_umin,  uint64_t, "ldat", _AMO_LD_UMIN)
_AMO_LD_SIMPLE (amo_ldat_swap,  uint64_t, "ldat", _AMO_LD_SWAP)

_AMO_LD_SIMPLE (amo_ldat_sadd,  int64_t,  "ldat", _AMO_LD_ADD)
_AMO_LD_SIMPLE (amo_ldat_smax,  int64_t,  "ldat", _AMO_LD_SMAX)
_AMO_LD_SIMPLE (amo_ldat_smin,  int64_t,  "ldat", _AMO_LD_SMIN)
_AMO_LD_SIMPLE (amo_ldat_sswap, int64_t,  "ldat", _AMO_LD_SWAP)

/* Enumeration of the STWAT/STDAT sub-opcodes.  */
enum _AMO_ST {
  _AMO_ST_ADD		= 0x00,		/* Store Add.  */
  _AMO_ST_XOR		= 0x01,		/* Store Xor.  */
  _AMO_ST_IOR		= 0x02,		/* Store Ior.  */
  _AMO_ST_AND		= 0x03,		/* Store And.  */
  _AMO_ST_UMAX		= 0x04,		/* Store Unsigned Maximum.  */
  _AMO_ST_SMAX		= 0x05,		/* Store Signed Maximum.  */
  _AMO_ST_UMIN		= 0x06,		/* Store Unsigned Minimum.  */
  _AMO_ST_SMIN		= 0x07,		/* Store Signed Minimum.  */
  _AMO_ST_TWIN		= 0x18		/* Store Twin.  */
};

/* Implementation of the simple STWAT/STDAT operations that take one register
   and modify one word or double-word of memory.  No value is returned.

   The STWAT/STDAT opcode requires the address to be a single register, and
   that points to a suitably aligned memory location.  Asm volatile is used to
   prevent the optimizer from moving the operation.  */

#define _AMO_ST_SIMPLE(NAME, TYPE, OPCODE, FC)				\
static __inline__ void							\
NAME (TYPE *_PTR, TYPE _VALUE)						\
{									\
  __asm__ volatile (OPCODE " %1,%P0,%2"					\
		    : "+Q" (_PTR[0])					\
		    : "r" (_VALUE), "n" (FC));				\
  return;								\
}

_AMO_ST_SIMPLE (amo_stwat_add,  uint32_t, "stwat", _AMO_ST_ADD)
_AMO_ST_SIMPLE (amo_stwat_xor,  uint32_t, "stwat", _AMO_ST_XOR)
_AMO_ST_SIMPLE (amo_stwat_ior,  uint32_t, "stwat", _AMO_ST_IOR)
_AMO_ST_SIMPLE (amo_stwat_and,  uint32_t, "stwat", _AMO_ST_AND)
_AMO_ST_SIMPLE (amo_stwat_umax, uint32_t, "stwat", _AMO_ST_UMAX)
_AMO_ST_SIMPLE (amo_stwat_umin, uint32_t, "stwat", _AMO_ST_UMIN)

_AMO_ST_SIMPLE (amo_stwat_sadd, int32_t,  "stwat", _AMO_ST_ADD)
_AMO_ST_SIMPLE (amo_stwat_smax, int32_t,  "stwat", _AMO_ST_SMAX)
_AMO_ST_SIMPLE (amo_stwat_smin, int32_t,  "stwat", _AMO_ST_SMIN)

_AMO_ST_SIMPLE (amo_stdat_add,  uint64_t, "stdat", _AMO_ST_ADD)
_AMO_ST_SIMPLE (amo_stdat_xor,  uint64_t, "stdat", _AMO_ST_XOR)
_AMO_ST_SIMPLE (amo_stdat_ior,  uint64_t, "stdat", _AMO_ST_IOR)
_AMO_ST_SIMPLE (amo_stdat_and,  uint64_t, "stdat", _AMO_ST_AND)
_AMO_ST_SIMPLE (amo_stdat_umax, uint64_t, "stdat", _AMO_ST_UMAX)
_AMO_ST_SIMPLE (amo_stdat_umin, uint64_t, "stdat", _AMO_ST_UMIN)

_AMO_ST_SIMPLE (amo_stdat_sadd, int64_t,  "stdat", _AMO_ST_ADD)
_AMO_ST_SIMPLE (amo_stdat_smax, int64_t,  "stdat", _AMO_ST_SMAX)
_AMO_ST_SIMPLE (amo_stdat_smin, int64_t,  "stdat", _AMO_ST_SMIN)
#endif	/* _ARCH_PWR9 && _ARCH_PPC64.  */
#endif	/* _POWERPC_AMO_H.  */
