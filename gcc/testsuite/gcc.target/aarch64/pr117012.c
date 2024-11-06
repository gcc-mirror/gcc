/* { dg-do compile } */
/* { dg-options "-O2" } */

#define vector16 __attribute__((vector_size(16)))

vector16 unsigned char
g (vector16 unsigned char a)
{
  vector16 signed char b = (vector16 signed char)a;
  b = b >> 7;
  vector16 unsigned char c = (vector16 unsigned char)b;
  vector16 unsigned char d = { 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0 };
  return c & d;
}

/* { dg-final { scan-assembler-times {and\tv[0-9]+\.16b, v[0-9]+\.16b, v[0-9]+\.16b} 1 } } */
