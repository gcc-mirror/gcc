// PR c++/60862
// { dg-do compile }

extern void **bar (int, void *, int);

void
foo (int x, int y)
{
  int **s = bar (x, &x, y); // { dg-error "17:invalid conversion" }
}
