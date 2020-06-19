/* { dg-do compile } */

typedef int a[10];
typedef struct {
  a b;
  a c;
  a d;
  a e;
} f;
f g;
int *j;
void k() {
  for (;;) {
    a l;
    j[0] = g.b[0];
    int *h = g.d;
    int i = 0;
    for (; i < 10; i++)
      h[i] = l[0] - g.e[0];
    h = g.e;
    i = 0;
    for (; i < 10; i++)
      h[i] = l[1] + g.e[i];
  }
}
