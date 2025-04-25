/* PR target/119873 */
/* { dg-do compile } */
/* { dg-options "-O2 -m31 -mzarch" } */

extern void foo (int x, int y, int z, long long w, int v);

void
bar (int x, int y, int z, long long w, int v)
{
  [[gnu::musttail]] return foo (x, y, z, w, v);
}
