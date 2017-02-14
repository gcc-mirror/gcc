/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */

void
foo (unsigned long long *a)
{
  a[0] = 0x0140c0da0140c0daULL;
}

/* { dg-final { scan-assembler-times "movk\\tw.*" 1 } } */
/* { dg-final { scan-assembler-times "stp\tw\[0-9\]+, w\[0-9\]+.*" 1 } } */
