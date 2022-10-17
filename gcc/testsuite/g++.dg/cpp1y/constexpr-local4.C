// { dg-do compile { target c++14 } }

struct A
{
  int i;
  constexpr A(int i): i(i) {};
};

const A a = 42;

constexpr int f()
{
  const int j = a.i;		// { dg-error "'a'" }
  return j;
}

static_assert (f() == 42,"");	// { dg-error "" }
