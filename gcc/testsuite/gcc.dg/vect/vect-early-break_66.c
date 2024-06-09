/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int a[0];
int b;

void g();

void f() {
  int d, e;
  for (; e; e++) {
    int c;
    switch (b)
    case '9': {
      for (; d < 1; d++)
        if (a[d])
          c = 1;
      break;
    case '<':
      g();
      c = 0;
    }
      while (c)
        ;
  }
}
