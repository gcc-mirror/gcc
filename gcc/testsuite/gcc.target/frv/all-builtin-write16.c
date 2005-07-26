/* { dg-do compile } */

unsigned short *addr;
unsigned short datum;

void foo ()
{
  __builtin_write16 (addr, datum);
}

/* { dg-final { scan-assembler "sth" } } */
/* { dg-final { scan-assembler "membar" } } */
