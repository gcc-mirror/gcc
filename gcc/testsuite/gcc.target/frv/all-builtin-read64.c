/* { dg-do compile } */

unsigned long long z;

void foo (void *x)
{
  z = __builtin_read64 (x);
}

/* { dg-final { scan-assembler "ldd" } } */
/* { dg-final { scan-assembler "membar" } } */
