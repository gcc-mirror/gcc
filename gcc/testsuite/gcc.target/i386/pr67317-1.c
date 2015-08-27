/* PR target/67317 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned int u32;

u32 testcarry_u32 (u32 a, u32 b, u32 c, u32 d)
{
  u32 result0, result1;

  __builtin_ia32_addcarryx_u32
    (__builtin_ia32_addcarryx_u32 (0, a, c, &result0), b, d, &result1);

  return result0 ^ result1;
}

/* { dg-final { scan-assembler-not "addb" } } */
/* { dg-final { scan-assembler-not "setn?c" } } */
