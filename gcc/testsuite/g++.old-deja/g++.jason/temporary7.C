// { dg-do run  }
// PRMS ID: 7304

struct V {
  int n;
  V() : n(0) { }
  V(int x) : n(x) { }
};

V baz(const V &x)
{
  return x;
}

int bar(V v1, V v2, V v3)
{
  return v1.n;
}

struct A {
  A(): n(7) { }
  int foo();
  V n;
};

int A::foo()
{
  V v1, v2;
  return bar(n, baz(v1), v2);
}

int main()
{
  A a;
  return (a.foo() != 7);
}
