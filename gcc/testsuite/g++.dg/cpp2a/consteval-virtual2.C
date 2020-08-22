// { dg-do compile { target c++20 } }

struct A
{
  virtual consteval int f() const { return 1; };
};

struct B: A
{
  virtual consteval int f() const { return 2; };
  virtual void g() { }
};

consteval int f()
{
  const A& ar = B();
  return ar.f();
}

static_assert (f() == 2);

B b;
