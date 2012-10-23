/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int functest(unsigned int x)
{
  return __builtin_ffs(x);
}

/* { dg-final { scan-assembler "cmp\tw" } } */
/* { dg-final { scan-assembler "rbit\tw" } } */
/* { dg-final { scan-assembler "clz\tw" } } */
/* { dg-final { scan-assembler "csinc\tw" } } */
