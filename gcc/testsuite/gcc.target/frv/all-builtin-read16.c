/* { dg-do compile } */

unsigned short z;

void foo (void *x)
{
  z = __builtin_read16 (x);
}

/* { dg-final { scan-assembler "lduh" } } */
/* { dg-final { scan-assembler "membar" } } */
