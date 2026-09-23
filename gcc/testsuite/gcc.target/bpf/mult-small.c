/* Verify that a multiply by a small constant uses BPF's native single
   instruction mul rather than a synthesized shift/add(-sub) sequence.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=v4" } */

unsigned
mul7 (unsigned x)
{
  return x * 7;
}

/* { dg-final { scan-assembler {\*= 7} } } */
/* { dg-final { scan-assembler-not {<<=} } } */
