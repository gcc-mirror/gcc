// { dg-do run { target c++11 } }

int a_ct;

struct A
{
  A(int i): i(i) { ++a_ct; }
  A(const A& a): i(a.i) { ++a_ct; }
  ~A() { --a_ct; }
  int i;
};

struct V
{
  V() { }
};

struct B: virtual V
{
  A a;
  B(A a): a(a) { }
  B(int i): B(A(i)) { }
};

struct C: B
{
  C(int i): B(i) { }
};

int main()
{
  {
    B b(42);
    C c(24);
    if (b.a.i != 42
	||c.a.i != 24)
      __builtin_abort ();
  }
  return a_ct;
}
