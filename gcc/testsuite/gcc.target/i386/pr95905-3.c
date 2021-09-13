/* PR target/95905 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */
/* { dg-final { scan-assembler-times "\tvpmovzxbw\t" 4 } } */
/* { dg-final { scan-assembler-times "\tvpmovzxwd\t" 4 } } */
/* { dg-final { scan-assembler-times "\tvpmovzxdq\t" 4 } } */

typedef unsigned char V1 __attribute__((vector_size (32)));
typedef unsigned short V2 __attribute__((vector_size (32)));
typedef unsigned int V3 __attribute__((vector_size (32)));

V1
f1 (V1 x)
{
  return __builtin_shuffle (x, (V1) {}, (V1) { 0, 32, 1, 33, 2, 34, 3, 35, 4, 36, 5, 37, 6, 38, 7, 39, 8, 40, 9, 41, 10, 42, 11, 43, 12, 44, 13, 45, 14, 46, 15, 47 });
}

V2
f2 (V2 x)
{
  return __builtin_shuffle (x, (V2) {}, (V2) { 0, 16, 1, 17, 2, 18, 3, 19, 4, 20, 5, 21, 6, 22, 7, 23 });
}

V3
f3 (V3 x)
{
  return __builtin_shuffle (x, (V3) {}, (V3) { 0, 8, 1, 9, 2, 10, 3, 11 });
}

V1
f4 (V1 *x)
{
  return __builtin_shuffle (*x, (V1) {}, (V1) { 0, 32, 1, 33, 2, 34, 3, 35, 4, 36, 5, 37, 6, 38, 7, 39, 8, 40, 9, 41, 10, 42, 11, 43, 12, 44, 13, 45, 14, 46, 15, 47 });
}

V2
f5 (V2 *x)
{
  return __builtin_shuffle (*x, (V2) {}, (V2) { 0, 16, 1, 17, 2, 18, 3, 19, 4, 20, 5, 21, 6, 22, 7, 23 });
}

V3
f6 (V3 *x)
{
  return __builtin_shuffle (*x, (V3) {}, (V3) { 0, 8, 1, 9, 2, 10, 3, 11 });
}

V1
f7 (V1 x)
{
  return __builtin_shuffle ((V1) {}, x, (V1) { 32, 0, 33, 1, 34, 2, 35, 3, 36, 4, 37, 5, 38, 6, 39, 7, 40, 8, 41, 9, 42, 10, 43, 11, 44, 12, 45, 13, 46, 14, 47, 15 });
}

V2
f8 (V2 x)
{
  return __builtin_shuffle ((V2) {}, x, (V2) { 16, 0, 17, 1, 18, 2, 19, 3, 20, 4, 21, 5, 22, 6, 23, 7 });
}

V3
f9 (V3 x)
{
  return __builtin_shuffle ((V3) {}, x, (V3) { 8, 0, 9, 1, 10, 2, 11, 3 });
}

V1
f10 (V1 *x)
{
  return __builtin_shuffle ((V1) {}, *x, (V1) { 32, 0, 33, 1, 34, 2, 35, 3, 36, 4, 37, 5, 38, 6, 39, 7, 40, 8, 41, 9, 42, 10, 43, 11, 44, 12, 45, 13, 46, 14, 47, 15 });
}

V2
f11 (V2 *x)
{
  return __builtin_shuffle ((V2) {}, *x, (V2) { 16, 0, 17, 1, 18, 2, 19, 3, 20, 4, 21, 5, 22, 6, 23, 7 });
}

V3
f12 (V3 *x)
{
  return __builtin_shuffle ((V3) {}, *x, (V3) { 8, 0, 9, 1, 10, 2, 11, 3 });
}
