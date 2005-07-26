/* { dg-do compile } */

unsigned char *addr;
unsigned char datum;

void foo ()
{
  __builtin_write8 (addr, datum);
}

/* { dg-final { scan-assembler "stb" } } */
/* { dg-final { scan-assembler "membar" } } */
