/* { dg-do compile } */

unsigned long *addr;
unsigned long datum;

void foo ()
{
  __builtin_write32 (addr, datum);
}

/* { dg-final { scan-assembler "st " } } */
/* { dg-final { scan-assembler "membar" } } */
