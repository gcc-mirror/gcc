/* PR target/117192 */
/* { dg-do run } */
/* { dg-options "-O3 -fno-unswitch-loops" } */

int a, b, c, d;
int main() {
  int e[6];
  for (d = 0; d < 6; d++)
    if (!c)
      e[d] = 0;
  for (; b < 6; b++)
    a = e[b];
  if (a != 0)
    __builtin_abort();
  return 0;
}
