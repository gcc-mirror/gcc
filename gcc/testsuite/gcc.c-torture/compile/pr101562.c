/* PR rtl-optimization/101562 */

struct S { char c; };
void baz (struct S a, struct S b);

void
foo (void)
{
  struct S x[1];
  *(short *)&x[0] = 256;
  baz (x[0], x[1]);
}

void
bar (void)
{
  struct S x[1];
  x[0].c = 0;
  x[1].c = 1;
  baz (x[0], x[1]);
}
