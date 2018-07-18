/* { dg-do compile } */
/* { dg-options "-Os" } */

/* Check that for -Os we synthesize only the bottom half and then
   store it twice with an STP rather than synthesizing it twice in each
   half of an X-reg.  */

void
foo (unsigned long long *a)
{
  a[0] = 0xc0da0000c0daULL;
}

/* { dg-final { scan-assembler-times "mov\\tw.*" 1 } } */
/* { dg-final { scan-assembler-times "stp\tw\[0-9\]+, w\[0-9\]+.*" 1 } } */
