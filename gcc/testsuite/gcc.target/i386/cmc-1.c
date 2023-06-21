/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int o1;
unsigned int o2;

unsigned int foo_xor (unsigned int a, unsigned int b,
                      unsigned int c, unsigned int d)
{
  unsigned int c1 = __builtin_ia32_addcarryx_u32 (1, a, b, &o1);
  return __builtin_ia32_addcarryx_u32 (c1 ^ 1, c, d, &o2);
}

unsigned int foo_sub (unsigned int a, unsigned int b,
                      unsigned int c, unsigned int d)
{
  unsigned int c1 = __builtin_ia32_addcarryx_u32 (1, a, b, &o1);
  return __builtin_ia32_addcarryx_u32 (1 - c1, c, d, &o2);
}

unsigned int foo_eqz (unsigned int a, unsigned int b,
                      unsigned int c, unsigned int d)
{
  unsigned int c1 = __builtin_ia32_addcarryx_u32 (1, a, b, &o1);
  return __builtin_ia32_addcarryx_u32 (c1 == 0, c, d, &o2);
}

/* { dg-final { scan-assembler "cmc" } } */
