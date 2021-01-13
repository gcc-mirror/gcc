/* PR target/95905 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */
/* { dg-final { scan-assembler "\tv?pmovzxbw\t" } } */
/* { dg-final { scan-assembler "\tv?pmovzxwd\t" } } */
/* { dg-final { scan-assembler "\tv?pmovzxdq\t" } } */

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
