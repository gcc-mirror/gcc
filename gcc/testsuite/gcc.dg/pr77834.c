/* PR target/77834 */
/* { dg-do compile } */
/* { dg-options "-O -ftree-pre -Wno-psabi" } */
/* { dg-additional-options "-mstringop-strategy=libcall" { target i?86-*-* x86_64-*-* } } */

typedef int V __attribute__ ((vector_size (64)));

V
foo (V u, V v, int w)
{
  do
    {
      if (u[0]) v ^= u[w];
    }
  while ((V) { 0, u[w] }[1]);
  u = (V) { v[v[0]], u[u[0]] };
  return v + u;
}
