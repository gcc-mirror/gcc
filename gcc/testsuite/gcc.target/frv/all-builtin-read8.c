/* { dg-do compile } */

unsigned char z;

void foo (void *x)
{
  z = __builtin_read8 (x);
}

/* { dg-final { scan-assembler "ldub" } } */
/* { dg-final { scan-assembler "membar" } } */
