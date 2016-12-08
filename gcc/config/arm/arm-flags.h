/* Flags used to identify the presence of processor capabilities.

   Copyright (C) 2016 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_ARM_FLAGS_H
#define GCC_ARM_FLAGS_H

/* Flags used to identify the presence of processor capabilities.  */

/* Bit values used to identify processor capabilities.  */
#define FL_NONE	      (0U)		/* No flags.  */
#define FL_ANY	      (0xffffffffU)	/* All flags.  */
#define FL_CO_PROC    (1U << 0)		/* Has external co-processor bus.  */
#define FL_ARCH3M     (1U << 1)		/* Extended multiply.  */
#define FL_MODE26     (1U << 2)		/* 26-bit mode support.  */
#define FL_MODE32     (1U << 3)		/* 32-bit mode support.  */
#define FL_ARCH4      (1U << 4)		/* Architecture rel 4.  */
#define FL_ARCH5      (1U << 5)		/* Architecture rel 5.  */
#define FL_THUMB      (1U << 6)		/* Thumb aware.  */
#define FL_LDSCHED    (1U << 7)		/* Load scheduling necessary.  */
#define FL_STRONG     (1U << 8)		/* StrongARM.  */
#define FL_ARCH5E     (1U << 9)		/* DSP extensions to v5.  */
#define FL_XSCALE     (1U << 10)	/* XScale.  */
/* spare	      (1U << 11) */
#define FL_ARCH6      (1U << 12)	/* Architecture rel 6.  Adds
					   media instructions.  */
#define FL_VFPV2      (1U << 13)	/* Vector Floating Point V2.  */
#define FL_WBUF	      (1U << 14)	/* Schedule for write buffer ops.
					   Note: ARM6 & 7 derivatives only.  */
#define FL_ARCH6K     (1U << 15)	/* Architecture rel 6 K extensions.  */
#define FL_THUMB2     (1U << 16)	/* Thumb-2.  */
#define FL_NOTM	      (1U << 17)	/* Instructions not present in the 'M'
					   profile.  */
#define FL_THUMB_DIV  (1U << 18)	/* Hardware divide (Thumb mode).  */
#define FL_VFPV3      (1U << 19)	/* Vector Floating Point V3.  */
#define FL_NEON       (1U << 20)	/* Neon instructions.  */
#define FL_ARCH7EM    (1U << 21)	/* Instructions present in the ARMv7E-M
					   architecture.  */
#define FL_ARCH7      (1U << 22)	/* Architecture 7.  */
#define FL_ARM_DIV    (1U << 23)	/* Hardware divide (ARM mode).  */
#define FL_ARCH8      (1U << 24)	/* Architecture 8.  */
#define FL_CRC32      (1U << 25)	/* ARMv8 CRC32 instructions.  */
#define FL_SMALLMUL   (1U << 26)	/* Small multiply supported.  */
#define FL_NO_VOLATILE_CE  (1U << 27)	/* No volatile memory in IT block.  */

#define FL_IWMMXT     (1U << 29)	/* XScale v2 or "Intel Wireless MMX
					   technology".  */
#define FL_IWMMXT2    (1U << 30)	/* "Intel Wireless MMX2
					    technology".  */
#define FL_ARCH6KZ    (1U << 31)	/* ARMv6KZ architecture.  */

#define FL2_ARCH8_1   (1U << 0)		/* Architecture 8.1.  */
#define FL2_ARCH8_2   (1U << 1)		/* Architecture 8.2.  */
#define FL2_FP16INST  (1U << 2)		/* FP16 Instructions for ARMv8.2 and
					   later.  */
#define FL2_CMSE      (1U << 3)		/* ARMv8-M Security Extensions.  */

/* Flags that only effect tuning, not available instructions.  */
#define FL_TUNE		(FL_WBUF | FL_VFPV2 | FL_STRONG | FL_LDSCHED \
			 | FL_CO_PROC)

#define FL_FOR_ARCH2		FL_NOTM
#define FL_FOR_ARCH3		(FL_FOR_ARCH2 | FL_MODE32)
#define FL_FOR_ARCH3M		(FL_FOR_ARCH3 | FL_ARCH3M)
#define FL_FOR_ARCH4		(FL_FOR_ARCH3M | FL_ARCH4)
#define FL_FOR_ARCH4T		(FL_FOR_ARCH4 | FL_THUMB)
#define FL_FOR_ARCH5		(FL_FOR_ARCH4 | FL_ARCH5)
#define FL_FOR_ARCH5T		(FL_FOR_ARCH5 | FL_THUMB)
#define FL_FOR_ARCH5E		(FL_FOR_ARCH5 | FL_ARCH5E)
#define FL_FOR_ARCH5TE		(FL_FOR_ARCH5E | FL_THUMB)
#define FL_FOR_ARCH5TEJ		FL_FOR_ARCH5TE
#define FL_FOR_ARCH6		(FL_FOR_ARCH5TE | FL_ARCH6)
#define FL_FOR_ARCH6J		FL_FOR_ARCH6
#define FL_FOR_ARCH6K		(FL_FOR_ARCH6 | FL_ARCH6K)
#define FL_FOR_ARCH6Z		FL_FOR_ARCH6
#define FL_FOR_ARCH6ZK		FL_FOR_ARCH6K
#define FL_FOR_ARCH6KZ		(FL_FOR_ARCH6K | FL_ARCH6KZ)
#define FL_FOR_ARCH6T2		(FL_FOR_ARCH6 | FL_THUMB2)
#define FL_FOR_ARCH6M		(FL_FOR_ARCH6 & ~FL_NOTM)
#define FL_FOR_ARCH7		((FL_FOR_ARCH6T2 & ~FL_NOTM) | FL_ARCH7)
#define FL_FOR_ARCH7A		(FL_FOR_ARCH7 | FL_NOTM | FL_ARCH6K)
#define FL_FOR_ARCH7VE		(FL_FOR_ARCH7A | FL_THUMB_DIV | FL_ARM_DIV)
#define FL_FOR_ARCH7R		(FL_FOR_ARCH7A | FL_THUMB_DIV)
#define FL_FOR_ARCH7M		(FL_FOR_ARCH7 | FL_THUMB_DIV)
#define FL_FOR_ARCH7EM		(FL_FOR_ARCH7M | FL_ARCH7EM)
#define FL_FOR_ARCH8A		(FL_FOR_ARCH7VE | FL_ARCH8)
#define FL2_FOR_ARCH8_1A	FL2_ARCH8_1
#define FL2_FOR_ARCH8_2A	(FL2_FOR_ARCH8_1A | FL2_ARCH8_2)
#define FL_FOR_ARCH8M_BASE	(FL_FOR_ARCH6M | FL_ARCH8 | FL_THUMB_DIV)
#define FL_FOR_ARCH8M_MAIN	(FL_FOR_ARCH7M | FL_ARCH8)

/* There are too many feature bits to fit in a single word so the set of cpu and
   fpu capabilities is a structure.  A feature set is created and manipulated
   with the ARM_FSET macros.  */

typedef struct
{
  unsigned cpu[2];
} arm_feature_set;


/* Initialize a feature set.  */

#define ARM_FSET_MAKE(CPU1,CPU2) { { (CPU1), (CPU2) } }

#define ARM_FSET_MAKE_CPU1(CPU1) ARM_FSET_MAKE ((CPU1), (FL_NONE))
#define ARM_FSET_MAKE_CPU2(CPU2) ARM_FSET_MAKE ((FL_NONE), (CPU2))

/* Accessors.  */

#define ARM_FSET_CPU1(S) ((S).cpu[0])
#define ARM_FSET_CPU2(S) ((S).cpu[1])

/* Useful combinations.  */

#define ARM_FSET_EMPTY ARM_FSET_MAKE (FL_NONE, FL_NONE)
#define ARM_FSET_ANY ARM_FSET_MAKE (FL_ANY, FL_ANY)

/* Tests for a specific CPU feature.  */

#define ARM_FSET_HAS_CPU1(A, F)  \
  (((A).cpu[0] & ((unsigned long)(F))) == ((unsigned long)(F)))
#define ARM_FSET_HAS_CPU2(A, F)  \
  (((A).cpu[1] & ((unsigned long)(F))) == ((unsigned long)(F)))
#define ARM_FSET_HAS_CPU(A, F1, F2)				\
  (ARM_FSET_HAS_CPU1 ((A), (F1)) && ARM_FSET_HAS_CPU2 ((A), (F2)))

/* Add a feature to a feature set.  */

#define ARM_FSET_ADD_CPU1(DST, F)		\
  do {						\
    (DST).cpu[0] |= (F);			\
  } while (0)

#define ARM_FSET_ADD_CPU2(DST, F)		\
  do {						\
    (DST).cpu[1] |= (F);			\
  } while (0)

/* Remove a feature from a feature set.  */

#define ARM_FSET_DEL_CPU1(DST, F)		\
  do {						\
    (DST).cpu[0] &= ~(F);			\
  } while (0)

#define ARM_FSET_DEL_CPU2(DST, F)		\
  do {						\
    (DST).cpu[1] &= ~(F);			\
  } while (0)

/* Union of feature sets.  */

#define ARM_FSET_UNION(DST,F1,F2)		\
  do {						\
    (DST).cpu[0] = (F1).cpu[0] | (F2).cpu[0];	\
    (DST).cpu[1] = (F1).cpu[1] | (F2).cpu[1];	\
  } while (0)

/* Intersection of feature sets.  */

#define ARM_FSET_INTER(DST,F1,F2)		\
  do {						\
    (DST).cpu[0] = (F1).cpu[0] & (F2).cpu[0];	\
    (DST).cpu[1] = (F1).cpu[1] & (F2).cpu[1];	\
  } while (0)

/* Exclusive disjunction.  */

#define ARM_FSET_XOR(DST,F1,F2)				\
  do {							\
    (DST).cpu[0] = (F1).cpu[0] ^ (F2).cpu[0];		\
    (DST).cpu[1] = (F1).cpu[1] ^ (F2).cpu[1];		\
  } while (0)

/* Difference of feature sets: F1 excluding the elements of F2.  */

#define ARM_FSET_EXCLUDE(DST,F1,F2)		\
  do {						\
    (DST).cpu[0] = (F1).cpu[0] & ~(F2).cpu[0];	\
    (DST).cpu[1] = (F1).cpu[1] & ~(F2).cpu[1];	\
  } while (0)

/* Test for an empty feature set.  */

#define ARM_FSET_IS_EMPTY(A)		\
  (!((A).cpu[0]) && !((A).cpu[1]))

/* Tests whether the cpu features of A are a subset of B.  */

#define ARM_FSET_CPU_SUBSET(A,B)					\
  ((((A).cpu[0] & (B).cpu[0]) == (A).cpu[0])				\
   && (((A).cpu[1] & (B).cpu[1]) == (A).cpu[1]))

#endif /* GCC_ARM_FLAGS_H */
