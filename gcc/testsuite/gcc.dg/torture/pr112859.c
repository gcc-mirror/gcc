/* { dg-do run } */
/* { dg-additional-options "-ftree-loop-distribution" } */

struct a {
  char b;
  int c;
} f, *i = &f;
static struct a e[4];
int *d, **g = &d;
static int h, j;
int main()
{
  for (; h < 1; h++) {
    struct a k = {1, 1};
    for (j = 0; j < 2; j++) {
      *i = e[h];
      e[h] = k;
    }
    *g = 0;
  }
  if (f.c != 1)
    __builtin_abort();
  return 0;
}
