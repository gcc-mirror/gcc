/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int functest (unsigned int x)
{
  return __builtin_clrsb (x);
}

/* { dg-final { scan-assembler "cls\tw" } } */
