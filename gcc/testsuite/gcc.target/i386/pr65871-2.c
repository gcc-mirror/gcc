/* { dg-do compile } */
/* { dg-options "-O2 -mbmi2" } */

int foo (unsigned int x, unsigned int y)
{
  if (__builtin_ia32_bzhi_si (x, y))
    return 1;

  return 0;
}

/* { dg-final { scan-assembler-not "test" } } */
