/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int functest (unsigned int x)
{
  return __builtin_ctz (x);
}

/* { dg-final { scan-assembler "rbit\tw" } } */
/* { dg-final { scan-assembler "clz\tw" } } */

