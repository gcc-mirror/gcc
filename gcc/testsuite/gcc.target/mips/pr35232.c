/* { dg-do run } */
/* { dg-mips-options "-O" } */

NOMIPS16 unsigned int
f1 (unsigned long long x)
{
  unsigned int r;
  asm ("# %0" : "=a" (r) : "0" (x));
  asm ("# %0" : "=h" (r) : "0" (r));
  return r;
}

int
main (void)
{
  return f1 (4) != 4;
}
