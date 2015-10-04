/* { dg-do compile } */
/* { dg-options "-O3 -march=haswell" } */

struct _GPart {
  int *g;
};
static int b, d, e;
int *c, *f, *g;
int a;

int fn2(int, int);

int fn1(int p1) {
  int h = fn2(p1, (int)(long)&e);
  for (; d < e; d++)
    if (f[d] != a)
      h += g ? g[f[d]] : 1;
  return h;
}

int main() {
  struct _GPart *i;
  for (; b < (int)(long)(i->g); b++)
    c[b] = fn1((int)(long)i->g);
}
