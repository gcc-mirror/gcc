/* Definitions for Bytecode Interpreter.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

This file is part of GNU CC.

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

#define MAXLITERALS 5

struct arityvec
{
  char ninputs;
  char noutputs;
  char nliterals;
  char literals[MAXLITERALS];
};

struct argtype
{
  int modealign;		/* Argument mode:alignment */
  int size;			/* Argument size, in bytes */
};

struct callinfo
{
  int nargs;			/* Number of arguments in call */
  struct argtype retvaltype;	/* Type of return value */
  struct argtype argtypes[1];	/* Argument types */
};
  
/* Structure describing a bytecode function.  If this changes, we also
   need to change expand_function_end () in bc-trans.c  */
struct bytecode
{
  int stacksize;		/* Depth required of evaluation stack.  */
  int localsize;		/* Size in bytes of local variables.  */
  unsigned char *pc0;		/* Initial program counter. */
  void **ptrlit;		/* Vector of (relocatable) pointer literals. */
  struct callinfo *callinfo;	/* Vector of procedure call type info. */
};


#define INTERP_BPC 8		/* Bits per char */
#define INTERP_BPI \
  (sizeof (int) * INTERP_BPC)	/* Bits per int */


#ifndef min
#define min(L, R)  ((L) < (R) ? (L) : (R))
#endif


/* bit field operations. */

/* Low (high) mask: int with low (high) N bits set */

#define LM(N)   ((1 << (N)) - 1)
#define HM(N)	((~LM (INTERP_BPI - (N))))


/* Sign-extend SIZE low bits of VALUE to integer (typeof VALUE)
   Signed bitfields are loaded from memory by the sxloadBI instruction,
   which first retrieves the bitfield with XFIELD and then sign extends
   it to an SItype. */

#define EXTEND(SIZE, VALUE)						      \
  ({ SUtype value = (SUtype) (VALUE);					      \
    (value & (1 << ((SIZE) - 1)) ? value | ~LM (SIZE) : value); })


/* Given OFFSET:SIZE for  a bitfield, calculate:

   [1] BYTE_OFFSET  = the byte offset of the bit field.
   [2] BIT_OFFSET   = the bit offset of the bit field (less than INTERP_BPC).
   [3] NBYTES       = the number of integral bytes in the bit field.
   [4] TRAILING_BITS= the number of trailing bits (less than INTERP_BPC).


   ,        ,        ,        ,        ,    (memory bytes)
                    ----------------        (bitfield)
   |        |       ||        |    |        (divisions)
        ^         ^       ^      ^
        |         |       |      |__ [4]  (bits)
        |         |       |_________ [3]  (bytes)
        |         |_________________ [2]  (bits)
        |___________________________ [1]  (bytes)


   The above applies to BYTE_LOW_ENDIAN machines. In BYTE_BIG_ENDIAN machines, the
   bit numbering is reversed (i.e. bit 0 is the sign bit).

   (Alright, so I drew this to keep my tongue in cheek while writing the code below,
    not because I'm into ASCII art.) */


#define BI_PARAMS(OFFSET, SIZE, BYTE_OFFSET, BIT_OFFSET, NBYTES, TRAILING_BITS)		\
  { BYTE_OFFSET = (OFFSET) / (INTERP_BPC);				\
    BIT_OFFSET = (OFFSET) % (INTERP_BPC);				\
    NBYTES = ((SIZE) - (INTERP_BPC - (BIT_OFFSET))) / INTERP_BPC;	\
    if ((NBYTES) < 0 || ((NBYTES) > 64)) 				\
      NBYTES = 0;				 			\
    if ((SIZE) + (BIT_OFFSET) <= INTERP_BPC)				\
      TRAILING_BITS = 0;						\
    else								\
      TRAILING_BITS = ((SIZE) - (INTERP_BPC - (BIT_OFFSET))) % INTERP_BPC; }


/* SHIFT_IN_BITS retrieves NBITS bits from SOURCE and shifts into
   DEST. The bit field starts OFFSET bits into SOURCE.

   OR_IN_BITS copies the NBITS low bits from VALUE into a the bitfield in
   DEST offset by OFFSET bits. */


#if BYTES_BIG_ENDIAN

#define SHIFT_IN_BITS(DEST, SOURCE, OFFSET, NBITS)		\
  (DEST = ((DEST) << (NBITS))					\
   | (LM ((NBITS))						\
      & ((SOURCE) >> (INTERP_BPC - (OFFSET) - (NBITS)))))

#define OR_IN_BITS(DEST, VALUE, OFFSET, NBITS)			\
  (DEST = ((DEST) & ~(LM ((NBITS)) << (INTERP_BPC - (OFFSET) - (NBITS))))	\
   | (((VALUE) & LM ((NBITS))) << (INTERP_BPC - (OFFSET) - (NBITS))))

#else

#define SHIFT_IN_BITS(DEST, SOURCE, OFFSET, NBITS)		\
  (DEST = ((DEST) << (NBITS))					\
   | (LM ((NBITS))						\
      & ((SOURCE) >> (OFFSET))))

#define OR_IN_BITS(DEST, VALUE, OFFSET, NBITS)			\
  (DEST = ((DEST) & ~(LM ((NBITS)) << (OFFSET)))		\
   | (((VALUE) & LM ((NBITS))) << (OFFSET)))

#endif


/* Procedure call; arguments are a pointer to the function to be called,
   a pointer to a place to store the return value, a pointer to a vector
   describing the type of procedure call, and the interpreter's stack pointer,
   which will point to the first of the arguments at this point.  */

#define CALL(FUNC, CALLDESC, RETVAL, SP) __call(FUNC, CALLDESC, RETVAL, SP)


/* Procedure return; arguments are a pointer to the calldesc for this
   function, and a pointer to the place where the value to be returned
   may be found.  Generally the MACHARGS above contain a machine dependent
   cookie that is used to determine where to jump to.  */

#define PROCRET(CALLDESC, RETVAL) return
