/* { dg-do run } */

typedef struct {
  int a;
  int b;
} c;

typedef struct {
  int a;
  int b;
  int d;
} e;

typedef struct {
  int f;
  int g;
} h;

typedef struct {
  h i[1];
  e coords[100];
} j;

struct k {
  j glyf;
} l;

int m, n;
double o;
e *q;
e r;

int s(c *v) {
  if (v[0].a == m)
    __builtin_abort();
  int t = v[0].a + v[2].b * (v[2].b - v[0].b),
      u = (2. + v[4].b - v[2].b) * (v[4].b - v[2].b);
  if (t <= 3 * u) {
    v[0] = v[4];
    return 1;
  }
  return 0;
}

void w(struct k *v) {
  c p[5];
  e *a = &v->glyf.coords[0];
  if (a->d)
    p[0].a = p[0].b = a->b;
  q = &r;
  o = p[0].b;
  while (v->glyf.i[0].g--) {
    q = q == &r ? a : q + 1;
    if (q->d)
      switch (n) {
      case 2:
        p[4].a = q->a;
        p[4].b = q->b;
        n = s(p);
      }
    else
      switch (n) {
      case 0:
        n = 1;
        break;
      case 1:
        p[2].b = q->b;
        n = 2;
        break;
      case 2:
        if (s(p))
          n = 1;
      }
  }
}

int main() {
  l.glyf.i[0] = (h){0, 26};
  l.glyf.coords[0] = (e){4, 2, 3};
  l.glyf.coords[3] = (e){2, 126, 3};
  l.glyf.coords[4] = (e){2, 206};
  l.glyf.coords[6] = (e){0, 308, 5};
  w(&l);
}
