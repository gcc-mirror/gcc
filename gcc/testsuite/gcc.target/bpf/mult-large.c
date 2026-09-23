/* Verify that a multiply by a complex constant uses BPF's native single
   instruction mul rather than a synthesized shift/add(-sub) sequence.
   This is extracted from memset-4.c but with 32-bit multiplicand vs. byte
   as the constant multiplier could genuinely trigger a mpy synthesis as
      'x | x<<8 | x<<16 | x<<24'.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned
bcast (unsigned x)
{
  return x * 0x01010101u;
}

/* { dg-final { scan-assembler {\*= 16843009} } } */
/* { dg-final { scan-assembler-not {<<=} } } */
