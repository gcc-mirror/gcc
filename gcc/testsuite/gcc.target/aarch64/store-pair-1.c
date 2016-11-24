/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=generic" } */

int f(int *a, int b)
{
  a[28] = 0;
  a[29] = b;
  a[31] = 0;
}

/* We should be able to produce store pair for the store of 28/29 store. */
/* { dg-final { scan-assembler "stp\tw(\[0-9\]+)\|(zr), w\[0-9\]+" } } */
