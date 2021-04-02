/* PR target/95905 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */
/* { dg-final { scan-assembler-times "\tv?pmovzxbw\t" 4 } } */
/* { dg-final { scan-assembler-times "\tv?pmovzxwd\t" 4 } } */
/* { dg-final { scan-assembler-times "\tv?pmovzxdq\t" 4 } } */

typedef unsigned char V1 __attribute__((vector_size (16)));
typedef unsigned short V2 __attribute__((vector_size (16)));
typedef unsigned int V3 __attribute__((vector_size (16)));

V1
f1 (V1 x)
{
  return __builtin_shuffle (x, (V1) {}, (V1) { 0, 16, 1, 17, 2, 18, 3, 19, 4, 20, 5, 21, 6, 22, 7, 23 });
}

V2
f2 (V2 x)
{
  return __builtin_shuffle (x, (V2) {}, (V2) { 0, 8, 1, 9, 2, 10, 3, 11 });
}

V3
f3 (V3 x)
{
  return __builtin_shuffle (x, (V3) {}, (V3) { 0, 4, 1, 5 });
}

V1
f4 (V1 *x)
{
  return __builtin_shuffle (*x, (V1) {}, (V1) { 0, 16, 1, 17, 2, 18, 3, 19, 4, 20, 5, 21, 6, 22, 7, 23 });
}

V2
f5 (V2 *x)
{
  return __builtin_shuffle (*x, (V2) {}, (V2) { 0, 8, 1, 9, 2, 10, 3, 11 });
}

V3
f6 (V3 *x)
{
  return __builtin_shuffle (*x, (V3) {}, (V3) { 0, 4, 1, 5 });
}

V1
f7 (V1 x)
{
  return __builtin_shuffle ((V1) {}, x, (V1) { 16, 0, 17, 1, 18, 2, 19, 3, 20, 4, 21, 5, 22, 6, 23, 7 });
}

V2
f8 (V2 x)
{
  return __builtin_shuffle ((V2) {}, x, (V2) { 8, 0, 9, 1, 10, 2, 11, 3 });
}

V3
f9 (V3 x)
{
  return __builtin_shuffle ((V3) {}, x, (V3) { 4, 0, 5, 1 });
}

V1
f10 (V1 *x)
{
  return __builtin_shuffle ((V1) {}, *x, (V1) { 16, 0, 17, 1, 18, 2, 19, 3, 20, 4, 21, 5, 22, 6, 23, 7 });
}

V2
f11 (V2 *x)
{
  return __builtin_shuffle ((V2) {}, *x, (V2) { 8, 0, 9, 1, 10, 2, 11, 3 });
}

V3
f12 (V3 *x)
{
  return __builtin_shuffle ((V3) {}, *x, (V3) { 4, 0, 5, 1 });
}
