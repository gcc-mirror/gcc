/* { dg-do compile } */

unsigned long long *addr;
unsigned long long datum;

void foo ()
{
  __builtin_write64 (addr, datum);
}

/* { dg-final { scan-assembler "std " } } */
/* { dg-final { scan-assembler "membar" } } */
