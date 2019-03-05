/* PR target/88234 */
/* { dg-do run { target { powerpc*-*-* && vmx_hw } } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-fsanitize=signed-integer-overflow -fno-sanitize-recover=signed-integer-overflow -O2 -maltivec" } */

#include <altivec.h>

__attribute__((noipa)) vector unsigned int
f1 (vector unsigned int x, vector unsigned int y)
{
  return vec_add (x, y);
}

__attribute__((noipa)) vector unsigned int
f2 (vector unsigned int x, vector unsigned int y)
{
  return vec_sub (x, y);
}

int
main ()
{
  vector unsigned int x = { __INT_MAX__, -__INT_MAX__, __INT_MAX__ - 3, -__INT_MAX__ + 4 };
  vector unsigned int y = { 1, -1, 4, -5 };
  vector unsigned int z = f1 (x, y);
  f2 (z, x);
  f2 (z, y);
  return 0;
}
