/* PR rtl-optimization/78547 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-Os -g -freorder-blocks-algorithm=simple -Wno-psabi" } */
/* { dg-additional-options "-mstringop-strategy=libcall" { target i?86-*-* x86_64-*-* } } */

typedef unsigned __int128 u128;
typedef unsigned __int128 V __attribute__ ((vector_size (64)));

V
foo (u128 a, u128 b, u128 c, V d)
{
  V e = (V) {a};
  V f = e & 1;
  e = 0 != e;
  c = c;
  f = f << ((V) {c} & 7);
  return f + e;
}
