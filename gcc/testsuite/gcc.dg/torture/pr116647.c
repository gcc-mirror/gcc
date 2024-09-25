/* { dg-do compile } */

int a;
char b;
long c, d, e;
unsigned long f;
long g() {
  if (a <= 0)
    return 1;
  for (; d; d++) {
    e = 0;
    for (; e < a; e++) {
      unsigned long h = 0;
      switch (b)
      case 2:
        if (e)
          h = 5;
      c += h;
    }
  }
  c /= f;
}
