/* { dg-do compile } */
/* { dg-options "-O" } */

unsigned long z;

void foo (void *x)
{
  z = __builtin_read32 (x);
}

/* { dg-final { scan-assembler "ld " } } */
/* { dg-final { scan-assembler "membar" } } */
