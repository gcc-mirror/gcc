/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned char u8x8 __attribute__((vector_size(8)));
typedef short i16x8 __attribute__((vector_size(16)));

u8x8
subv (u8x8 a, u8x8 b)
{
  i16x8 x = __builtin_convertvector (a, i16x8);
  i16x8 y = __builtin_convertvector (b, i16x8);
  i16x8 d = x - y;
  i16x8 zero = { 0, 0, 0, 0, 0, 0, 0, 0 };
  i16x8 r = d < zero ? zero : d;
  return __builtin_convertvector (r, u8x8);
}

u8x8
addv (u8x8 a, u8x8 b)
{
  i16x8 x = __builtin_convertvector (a, i16x8);
  i16x8 y = __builtin_convertvector (b, i16x8);
  i16x8 s = x + y;
  i16x8 max = { 255, 255, 255, 255, 255, 255, 255, 255 };
  i16x8 r = s > max ? max : s;
  return __builtin_convertvector (r, u8x8);
}

/* { dg-final { scan-tree-dump-times "MIN_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "MAX_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump-not "i16x8" "optimized" } } */
/* { dg-final { scan-assembler-times {\tumin\tv[0-9]+\.8b} 2 } } */
/* { dg-final { scan-assembler-times {\tsub\tv[0-9]+\.8b} 1 } } */
/* { dg-final { scan-assembler-times {\tnot\tv[0-9]+\.8b} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tv[0-9]+\.8b} 1 } } */
