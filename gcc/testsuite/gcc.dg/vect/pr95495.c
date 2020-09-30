/* { dg-do compile } */

typedef struct {
  int a;
  int b;
} c;
int d, f, g;
c e[4];
void
h()
{
  for (; f; f++) {
    g += e[f].a >> 1 | e[f].a & 1;
    d += e[f].b >> 1 | e[f].b & 1;
  }
}

/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 0 "vect" } } */
