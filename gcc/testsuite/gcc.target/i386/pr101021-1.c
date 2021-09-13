/* PR target/101021 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler-not "vpshufb" } } */

typedef char S;
typedef S V __attribute__((vector_size(16 * sizeof(S))));

V t1 (V x)
{
  return __builtin_shuffle (x, (V) { 8,9,10,11,12,13,14,15, 0,1,2,3,4,5,6,7 });
}

/* { dg-final { scan-assembler "vpalignr" } } */

V t2 (V x)
{
  return __builtin_shuffle (x, (V) { 0,1,2,3, 4,5,6,7, 4,5,6,7, 12,13,14,15 });
}

/* { dg-final { scan-assembler "vpshufd" } } */

V t3 (V x)
{
  return __builtin_shuffle (x, (V) { 0,1, 2,3, 2,3, 6,7, 8,9,10,11,12,13,14,15 });
}

/* { dg-final { scan-assembler "vpshuflw" } } */

V t4 (V x)
{
  return __builtin_shuffle (x, (V) { 0,1,2,3,4,5,6,7, 8,9, 10,11, 10,11, 14,15 });
}

/* { dg-final { scan-assembler "vpshufhw" } } */
