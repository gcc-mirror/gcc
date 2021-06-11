/* PR target/101021 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler-not "vpshufb" } } */

typedef char S;
typedef S V __attribute__((vector_size(8 * sizeof(S))));

V t1 (V x)
{
  return __builtin_shuffle (x, (V) { 4,5,6,7, 0,1,2,3 });
}

/* { dg-final { scan-assembler "vpshufd" } } */

V t2 (V x)
{
  return __builtin_shuffle (x, (V) { 0,1, 2,3, 2,3, 6,7 });
}

/* { dg-final { scan-assembler "vpshuflw" } } */
