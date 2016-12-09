/* PR tree-optimization/78691 */
/* { dg-options "-Os" } */

int *b;
int fn1() {
  char *c;
  int a;
  for (;;)
    switch (*b) {
    case 'c':
      while (--a > 0)
        *c++ = ' ';
      c++;
      if (a)
        a = sizeof(void *);
    }
}

