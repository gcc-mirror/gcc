/* { dg-do compile } */
/* { dg-options "-O2 -mlasx" } */
/* { dg-final { scan-assembler "\tvrepli\\.b\t\\\$vr\[0-9\]+,-35" } } */
/* { dg-final { scan-assembler "\txvrepli\\.b\t\\\$xr\[0-9\]+,-35" } } */

int f __attribute__((vector_size (16)));
int g __attribute__((vector_size (32)));

void
test (void)
{
  constexpr int x = (int) 0xdddddddd;
  f = (typeof(f)){x, x, x, x};
  g = (typeof(g)){x, x, x, x, x, x, x, x};
}
