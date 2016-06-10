/* { dg-do compile } */

int a, b, d, e, f, g;
int *c;
void fn2 (int *);
void fn1() { fn2(&e); }

void fn2(int *p1) {
  for (;;) {
    for (; a; a++)
      if (*p1 = g || --f, b)
        if (*c)
          d = *p1;
    if (*p1)
      break;
  }
}
