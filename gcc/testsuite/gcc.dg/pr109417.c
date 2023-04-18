/* { dg-do compile } */
/* { dg-options "-O3" } */

int printf(const char *, ...);
int c, d, *e, f[1][2], g;
int main() {
  int h = 0, *a = &h, **b[1] = {&a};
  while (e)
    while (g) {
    L:
      for (h = 0; h < 2; h++) {
        while (d)
          for (*e = 0; *e < 1;)
            printf("0");
        while (c)
          ;
        f[g][h] = 0;
      }
    }
  if (h)
    goto L;
  return 0;
}

