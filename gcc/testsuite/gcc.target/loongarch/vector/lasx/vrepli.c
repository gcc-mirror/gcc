/* { dg-do compile } */
/* { dg-options "-O2 -mlasx" } */
/* { dg-final { scan-assembler "\tvldi\t\\\$vr\[0-9\]+,221" } } */
/* { dg-final { scan-assembler "\txvldi\t\\\$xr\[0-9\]+,221" } } */

int f __attribute__((vector_size (16)));
int g __attribute__((vector_size (32)));

void
test (void)
{
  constexpr int x = (int) 0xdddddddd;
  f = (typeof(f)){x, x, x, x};
  g = (typeof(g)){x, x, x, x, x, x, x, x};
}
