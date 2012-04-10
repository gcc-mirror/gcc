// { dg-do compile }
// { dg-options "-g -Os" }

int grow (int);
void fn (int);
struct A { int a1, a2; };
template <typename T>
struct B
{
  A *b;
  ~B () { b3 (b); }
  void b1 (int);
  void b2 (int);
  void b3 (A *);
};
struct C { int c1, c2, c3; bool c4; };
int
bar (int x)
{
  int y = x / 6;
  if (y > x / 2)
    return y;
  return 0;
}
void baz (double, double);
void
foo (const C *x, int y, int z)
{
  B<int> p;
  double r = y / 2;
  int w = bar (int (r));
  double s = y / 2 + 0.5;
  double t = z / 2 + 0.5;
  int u = x->c3;
  int v = (x->c2 + u - 1 - x->c1) / u;
  p.b2 ((2 * v > p.b->a1 || (2 * v < p.b->a2 && 2 * v < (p.b->a1 >> 1)))
        ? grow (0) : p.b->a1);
  for (int i = 0; i <= v; ++i)
    {
      double l = x->c4 ? 4.5 - i * 6.2 / v : (3.1 - i * 31 / v) / 6;
      baz (s + (r - w) * l, t - (r - w) * l);
    }
}


