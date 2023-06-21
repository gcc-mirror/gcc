/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned int u32;

unsigned int foo (unsigned int a, unsigned int b, unsigned int *c)
{
  return __builtin_ia32_addcarryx_u32 (1, a, b, c);
}

unsigned int bar (unsigned int b, unsigned int *c)
{
  return __builtin_ia32_addcarryx_u32 (1, 2, b, c);
}

unsigned int baz (unsigned int a, unsigned int *c)
{
  return __builtin_ia32_addcarryx_u32 (1, a, 3, c);
}

/* { dg-final { scan-assembler "stc" } } */
