/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=x86-64-v4 -O2" } */
/* { dg-final { scan-assembler-not "vpextrq" } } */
/* { dg-final { scan-assembler-not "vpinsrq" } } */

typedef int v16si __attribute__((vector_size(64)));
typedef int v4si __attribute__((vector_size(16)));

v4si f(v16si x)
{
  return __builtin_shufflevector(x, x, 0, 1, 2, 3);
}

v4si g(v16si x)
{
return __builtin_shufflevector(x, x, 4, 5, 6, 7);
}

v4si f1(__int128 *x)
{
  __int128 t = *x;
  asm("":"+x"(t));
  return (v4si)t;
}
