/* Sets (bit vectors) of hard registers, and operations on them.
   Copyright (C) 1987, 1992 Free Software Foundation, Inc.

This file is part of GNU CC

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* Define the type of a set of hard registers.  */

/* If HARD_REG_SET is a macro, its definition is a scalar type
   that has enough bits for all the target machine's hard registers.
   Otherwise, it is a typedef for a suitable array of HOST_WIDE_INTs,
   and HARD_REG_SET_LONGS is how many.

   Note that lots of code assumes that the first part of a regset is
   the same format as a HARD_REG_SET.  To help make sure this is true,
   we only try the widest integer mode (HOST_WIDE_INT) instead of all the
   smaller types.  This only loses if there are a very few registers and
   then only in the few cases where we have an array of HARD_REG_SETs,
   so it isn't worth making this as complex as it used to be.  */

#if FIRST_PSEUDO_REGISTER <= HOST_BITS_PER_WIDE_INT
#define HARD_REG_SET HOST_WIDE_INT

#else

#define HARD_REG_SET_LONGS \
 ((FIRST_PSEUDO_REGISTER + HOST_BITS_PER_WIDE_INT - 1)	\
  / HOST_BITS_PER_WIDE_INT)
typedef HOST_WIDE_INT HARD_REG_SET[HARD_REG_SET_LONGS];

#endif

/* HARD_CONST is used to cast a constant to a HARD_REG_SET
   if that is a scalar wider than an integer.  */

#ifdef HARD_REG_SET
#define HARD_CONST(X) ((HARD_REG_SET) (X))
#else
#define HARD_CONST(X) (X)
#endif

/* Define macros SET_HARD_REG_BIT, CLEAR_HARD_REG_BIT and TEST_HARD_REG_BIT
   to set, clear or test one bit in a hard reg set of type HARD_REG_SET.
   All three take two arguments: the set and the register number.

   In the case where sets are arrays of longs, the first argument
   is actually a pointer to a long.

   Define two macros for initializing a set:
   CLEAR_HARD_REG_SET and SET_HARD_REG_SET.
   These take just one argument.

   Also define macros for copying hard reg sets:
   COPY_HARD_REG_SET and COMPL_HARD_REG_SET.
   These take two arguments TO and FROM; they read from FROM
   and store into TO.  COMPL_HARD_REG_SET complements each bit.

   Also define macros for combining hard reg sets:
   IOR_HARD_REG_SET and AND_HARD_REG_SET.
   These take two arguments TO and FROM; they read from FROM
   and combine bitwise into TO.  Define also two variants
   IOR_COMPL_HARD_REG_SET and AND_COMPL_HARD_REG_SET
   which use the complement of the set FROM.

   Also define GO_IF_HARD_REG_SUBSET (X, Y, TO):
   if X is a subset of Y, go to TO.
*/

#ifdef HARD_REG_SET

#define SET_HARD_REG_BIT(SET, BIT)  \
 ((SET) |= HARD_CONST (1) << (BIT))
#define CLEAR_HARD_REG_BIT(SET, BIT)  \
 ((SET) &= ~(HARD_CONST (1) << (BIT)))
#define TEST_HARD_REG_BIT(SET, BIT)  \
 ((SET) & (HARD_CONST (1) << (BIT)))

#define CLEAR_HARD_REG_SET(TO) ((TO) = HARD_CONST (0))
#define SET_HARD_REG_SET(TO) ((TO) = HARD_CONST (-1))

#define COPY_HARD_REG_SET(TO, FROM) ((TO) = (FROM))
#define COMPL_HARD_REG_SET(TO, FROM) ((TO) = ~(FROM))

#define IOR_HARD_REG_SET(TO, FROM) ((TO) |= (FROM))
#define IOR_COMPL_HARD_REG_SET(TO, FROM) ((TO) |= ~ (FROM))
#define AND_HARD_REG_SET(TO, FROM) ((TO) &= (FROM))
#define AND_COMPL_HARD_REG_SET(TO, FROM) ((TO) &= ~ (FROM))

#define GO_IF_HARD_REG_SUBSET(X,Y,TO) if (HARD_CONST (0) == ((X) & ~(Y))) goto TO

#define GO_IF_HARD_REG_EQUAL(X,Y,TO) if ((X) == (Y)) goto TO
#else

#define UHOST_BITS_PER_WIDE_INT ((unsigned) HOST_BITS_PER_WIDE_INT)

#define SET_HARD_REG_BIT(SET, BIT)		\
  ((SET)[(BIT) / UHOST_BITS_PER_WIDE_INT]	\
   |= (HOST_WIDE_INT) 1 << ((BIT) % UHOST_BITS_PER_WIDE_INT))

#define CLEAR_HARD_REG_BIT(SET, BIT)		\
  ((SET)[(BIT) / UHOST_BITS_PER_WIDE_INT]	\
   &= ~((HOST_WIDE_INT) 1 << ((BIT) % UHOST_BITS_PER_WIDE_INT)))

#define TEST_HARD_REG_BIT(SET, BIT)		\
  ((SET)[(BIT) / UHOST_BITS_PER_WIDE_INT]	\
   & ((HOST_WIDE_INT) 1 << ((BIT) % UHOST_BITS_PER_WIDE_INT)))

#define CLEAR_HARD_REG_SET(TO)  \
do { register HOST_WIDE_INT *scan_tp_ = (TO);			\
     register int i;						\
     for (i = 0; i < HARD_REG_SET_LONGS; i++)			\
       *scan_tp_++ = 0; } while (0)

#define SET_HARD_REG_SET(TO)  \
do { register HOST_WIDE_INT *scan_tp_ = (TO);			\
     register int i;						\
     for (i = 0; i < HARD_REG_SET_LONGS; i++)			\
       *scan_tp_++ = -1; } while (0)

#define COPY_HARD_REG_SET(TO, FROM)  \
do { register HOST_WIDE_INT *scan_tp_ = (TO), *scan_fp_ = (FROM); \
     register int i;						\
     for (i = 0; i < HARD_REG_SET_LONGS; i++)			\
       *scan_tp_++ = *scan_fp_++; } while (0)

#define COMPL_HARD_REG_SET(TO, FROM)  \
do { register HOST_WIDE_INT *scan_tp_ = (TO), *scan_fp_ = (FROM); \
     register int i;						\
     for (i = 0; i < HARD_REG_SET_LONGS; i++)			\
       *scan_tp_++ = ~ *scan_fp_++; } while (0)

#define AND_HARD_REG_SET(TO, FROM)  \
do { register HOST_WIDE_INT *scan_tp_ = (TO), *scan_fp_ = (FROM); \
     register int i;						\
     for (i = 0; i < HARD_REG_SET_LONGS; i++)			\
       *scan_tp_++ &= *scan_fp_++; } while (0)

#define AND_COMPL_HARD_REG_SET(TO, FROM)  \
do { register HOST_WIDE_INT *scan_tp_ = (TO), *scan_fp_ = (FROM); \
     register int i;						\
     for (i = 0; i < HARD_REG_SET_LONGS; i++)			\
       *scan_tp_++ &= ~ *scan_fp_++; } while (0)

#define IOR_HARD_REG_SET(TO, FROM)  \
do { register HOST_WIDE_INT *scan_tp_ = (TO), *scan_fp_ = (FROM); \
     register int i;						\
     for (i = 0; i < HARD_REG_SET_LONGS; i++)			\
       *scan_tp_++ |= *scan_fp_++; } while (0)

#define IOR_COMPL_HARD_REG_SET(TO, FROM)  \
do { register HOST_WIDE_INT *scan_tp_ = (TO), *scan_fp_ = (FROM); \
     register int i;						\
     for (i = 0; i < HARD_REG_SET_LONGS; i++)			\
       *scan_tp_++ |= ~ *scan_fp_++; } while (0)

#define GO_IF_HARD_REG_SUBSET(X,Y,TO)  \
do { register HOST_WIDE_INT *scan_xp_ = (X), *scan_yp_ = (Y);	\
     register int i;						\
     for (i = 0; i < HARD_REG_SET_LONGS; i++)			\
       if (0 != (*scan_xp_++ & ~*scan_yp_++)) break;		\
     if (i == HARD_REG_SET_LONGS) goto TO; } while (0)

#define GO_IF_HARD_REG_EQUAL(X,Y,TO)  \
do { register HOST_WIDE_INT *scan_xp_ = (X), *scan_yp_ = (Y);	\
     register int i;						\
     for (i = 0; i < HARD_REG_SET_LONGS; i++)			\
       if (*scan_xp_++ != ~*scan_yp_++)) break;			\
     if (i == HARD_REG_SET_LONGS) goto TO; } while (0)

#endif

/* Define some standard sets of registers.  */

/* Indexed by hard register number, contains 1 for registers
   that are fixed use (stack pointer, pc, frame pointer, etc.).
   These are the registers that cannot be used to allocate
   a pseudo reg whose life does not cross calls.  */

extern char fixed_regs[FIRST_PSEUDO_REGISTER];

/* The same info as a HARD_REG_SET.  */

extern HARD_REG_SET fixed_reg_set;

/* Indexed by hard register number, contains 1 for registers
   that are fixed use or are clobbered by function calls.
   These are the registers that cannot be used to allocate
   a pseudo reg whose life crosses calls.  */

extern char call_used_regs[FIRST_PSEUDO_REGISTER];

/* The same info as a HARD_REG_SET.  */

extern HARD_REG_SET call_used_reg_set;
  
/* Indexed by hard register number, contains 1 for registers that are
   fixed use -- i.e. in fixed_regs -- or a function value return register
   or STRUCT_VALUE_REGNUM or STATIC_CHAIN_REGNUM.  These are the
   registers that cannot hold quantities across calls even if we are
   willing to save and restore them.  */

extern char call_fixed_regs[FIRST_PSEUDO_REGISTER];

/* The same info as a HARD_REG_SET.  */

extern HARD_REG_SET call_fixed_reg_set;

/* Indexed by hard register number, contains 1 for registers
   that are being used for global register decls.
   These must be exempt from ordinary flow analysis
   and are also considered fixed.  */

extern char global_regs[FIRST_PSEUDO_REGISTER];

/* Table of register numbers in the order in which to try to use them.  */

#ifdef REG_ALLOC_ORDER   /* Avoid undef symbol in certain broken linkers.  */
extern int reg_alloc_order[FIRST_PSEUDO_REGISTER];
#endif

/* For each reg class, a HARD_REG_SET saying which registers are in it.  */

extern HARD_REG_SET reg_class_contents[];

/* For each reg class, number of regs it contains.  */

extern int reg_class_size[N_REG_CLASSES];

/* For each reg class, table listing all the containing classes.  */

extern enum reg_class reg_class_superclasses[N_REG_CLASSES][N_REG_CLASSES];

/* For each reg class, table listing all the classes contained in it.  */

extern enum reg_class reg_class_subclasses[N_REG_CLASSES][N_REG_CLASSES];

/* For each pair of reg classes,
   a largest reg class contained in their union.  */

extern enum reg_class reg_class_subunion[N_REG_CLASSES][N_REG_CLASSES];

/* For each pair of reg classes,
   the smallest reg class that contains their union.  */

extern enum reg_class reg_class_superunion[N_REG_CLASSES][N_REG_CLASSES];

/* Number of non-fixed registers.  */

extern int n_non_fixed_regs;

/* Vector indexed by hardware reg giving its name.  */

extern char *reg_names[FIRST_PSEUDO_REGISTER];
