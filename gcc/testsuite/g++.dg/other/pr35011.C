// { dg-do compile }
// { dg-options "-O3 -fcheck-data-deps" }

double foo(const double* p0, const double* p1, const double* q0)
{
  double d;
  for (; p0 != p1; ++p0, ++q0)
    d += *p0 * *q0;
  return d;
}

struct A
{
  double x[3];
  const double* begin() const { return x; }
};

struct B
{
  A a0, a1;
  double d;
  B(const A&);
};

B::B(const A& a) : a0(a), a1(a), d(foo(a0.begin(), a0.begin()+3, a1.begin()))
{}
