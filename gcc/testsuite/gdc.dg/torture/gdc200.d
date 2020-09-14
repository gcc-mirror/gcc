// https://bugzilla.gdcproject.org/show_bug.cgi?id=200
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void test200a(double x, double y)
{
  const double y2 = x + 1.0;
  assert(y == y2);
}

void main()
{
  const double x = .012;
  const double y = x + 1.0;
  test200a(x, y);
}
