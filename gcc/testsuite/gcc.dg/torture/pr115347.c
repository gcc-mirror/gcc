/* { dg-do run } */
/* { dg-additional-options "-ftree-loop-distribution" } */

struct a {
  int b;
  int c;
} d, e[2];
int f, g, h;
int main()
{
  for (; f < 1; f++) {
    for (h = 0; h < 2; h++) {
      d = e[f];
      g = e[1].c;
      e[f].c = 1;
    }
  }
  if (d.c != 1)
    __builtin_abort();
  return 0;
}
