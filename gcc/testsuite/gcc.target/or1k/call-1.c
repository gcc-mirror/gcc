/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=large" } */

/* Generate local and global function calls. */

extern int geti (void);

__attribute__ ((noinline)) int
calc (int a, int b)
{
  return a * b + 255;
}

int
main (void)
{
  return geti () + calc (3, 4);
}

/* Ensure the 2 calls use register not immediate jumps.  */
/* { dg-final { scan-assembler-times "l.movhi\\s+" 2 } } */
/* { dg-final { scan-assembler-times "l.jalr\\s+" 2 } } */
