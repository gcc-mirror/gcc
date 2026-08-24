/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

/* Verify that a 2048-bit __dmr2048 value held in a DMR pair is spilled to
   the stack and filled back when it is live across a call.  */

typedef __dmr2048 qdo_t;

extern void clobber (void);

void
spill (qdo_t *dst)
{
  qdo_t v;

  /* Force the value into a DMR pair, then keep it live across the call
     so that it has to be spilled to the stack and filled back again.  */
  asm volatile ("" : "=wD" (v));
  clobber ();
  *dst = v;
}

/* The spilled value is moved from the DMR pair to VSX registers with a
   sequence of four instructions, and is spilled to and filled from the
   stack with eight vector-pair accesses.  The stack addresses used to
   spill/fill the two halves are ABI dependent, so only the accesses
   through the stack pointer are checked here.  */
/* { dg-final { scan-assembler-times {\mdmxxextfdmr512\M} 4 } } */
/* { dg-final { scan-assembler-times {\mstxvp\s+[0-9]+,[0-9]+\(1\)} 8 } } */
/* { dg-final { scan-assembler-times {\mlxvp\s+[0-9]+,[0-9]+\(1\)} 8 } } */

/* Storing the filled value also checks the byte order of the two halves:
   in little endian mode the first half goes to the higher address, while
   in big endian mode it goes to the lower address.  */
/* { dg-final { scan-assembler-times {\mstxvp\s+3[0-9],128\([0-9]+\)} 1 { target { powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mstxvp\s+4[0-7],0\([0-9]+\)} 1 { target { powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mstxvp\s+3[0-9],0\([0-9]+\)} 1 { target { ! powerpc64le-*-* } } } } */
/* { dg-final { scan-assembler-times {\mstxvp\s+4[0-7],128\([0-9]+\)} 1 { target { ! powerpc64le-*-* } } } } */
