/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" { target sse4 } } */

int eq(unsigned long *x, unsigned long *y)
{
    unsigned long folded = 0;
    for (int i = 0; i < 4; ++i)
      folded |= x[i] ^ y[i];
    return folded == 0;
}

/* We want to elide the .REDUC_IOR with the compare against zero.  */
/* { dg-final { scan-assembler "ptest" } } */
