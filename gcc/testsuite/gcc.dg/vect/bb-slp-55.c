/* { dg-do compile } */

typedef struct {
  int a;
  int b;
  int c;
  int d;
} e;
e *f;
int g;
void h() {
  e *i;
  if (g) {
    i->c = f[g].b;
    i->d = f[g].a;
  } else
    i->c = i->d = 0;
}
