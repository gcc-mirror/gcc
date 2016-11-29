/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times "bset" 1 } } */
/* { dg-final { scan-assembler-times "bchg" 1 } } */
/* { dg-final { scan-assembler-times "bclr" 1 } } */

int bar (void);

int
foo1 (int b)
{
  int a = bar ();
  return ( a | (1 << (b & 31)));
}

int
foo2 (int b)
{
  int a = bar ();
  return ( a ^ (1 << (b & 31)));
}


int
foo3 (int b)
{
  int a = bar ();
  return ( a & ~(1 << (b & 31)));
}


