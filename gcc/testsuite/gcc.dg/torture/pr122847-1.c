/* { dg-do compile } */
/* PR tree-optimization/122847 */	

struct {
  char x[6];
} *a, b;

int c;

int d() {
  /* `a->x` might trap. */
  char *p = a ? a->x : b.x;
  char e = *p;
  if (c)
    return *(short *)p & e;
  return 0;
}
