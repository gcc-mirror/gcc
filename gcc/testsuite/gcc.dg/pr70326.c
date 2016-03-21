/* PR middle-end/70326 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-Og -fno-dce -fno-guess-branch-probability -fschedule-insns -fno-tree-coalesce-vars -fno-tree-dce -w -Wno-psabi" } */

typedef unsigned int A __attribute__ ((vector_size (32)));
typedef unsigned __int128 B;
typedef unsigned __int128 C __attribute__ ((vector_size (32)));
typedef unsigned __int128 D __attribute__ ((vector_size (32)));

void
foo (B a, D b, A c, A d, C e)
{
  b /= (D) {11} | 1;
  a ^= a <= 10;
  e *= (C) d;
  e += (C) ~b;
  c[0] ^= c[0] <= 0x1234;
  a = (a >> 1) | (a << 127);
  b += (D) {45, 123};
}
