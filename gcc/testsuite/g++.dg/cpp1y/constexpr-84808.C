// PR c++/84808
// { dg-do compile { target c++14 } }

struct A { int i; constexpr A () : i() {} };
struct B { A a[24]; };

constexpr int
foo (int n)
{
  B b;
  ++b.a[n + 20].i;
  ++b.a[n + 18].i;
  ++b.a[n + 16].i;
  ++b.a[n + 14].i;
  ++b.a[n + 12].i;
  ++b.a[n + 10].i;
  ++b.a[n + 8].i;
  ++b.a[n + 6].i;
  ++b.a[n + 4].i;
  ++b.a[n + 2].i;
  ++b.a[n].i;
  return b.a[2].i + b.a[4].i + b.a[6].i + b.a[8].i + b.a[10].i
	 + b.a[12].i + b.a[14].i + b.a[16].i + b.a[18].i + b.a[20].i + b.a[22].i;
}

constexpr int i = foo (2);
static_assert (i == 11, "");
