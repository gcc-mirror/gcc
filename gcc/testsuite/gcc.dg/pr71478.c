/* PR middle-end/71478 */
/* { dg-do compile } */
/* { dg-options "-O3 -Wno-psabi -w" } */

typedef unsigned int __attribute__ ((vector_size (8))) uv2si;
typedef int __attribute__ ((vector_size (8))) v2si;

uv2si bar (v2si);

uv2si
foo (void)
{
  v2si x = (v2si) (0x00007fff80008000UL);
  v2si y = (v2si) (0x8f997fff00000000UL);
  uv2si z = x >= y;
  uv2si k = bar (x);
  uv2si j = k * __builtin_shuffle (z, z, (uv2si) {1, 3});
  return k * j;
}
