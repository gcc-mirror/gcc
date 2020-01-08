// PR c++/92878 - Parenthesized init of aggregates in new-expression.
// { dg-do compile { target c++2a } }
// Test new TYPE(...).

int f ();

struct A
{
  int a;
  int b;
};

void
fn_A ()
{
  int i = 0;
  auto y = new A(1, 2);
  auto x = new A(++i, ++i);
  auto z = new A(1, { ++i });
  auto u = new A(1, f());
}

struct B
{
  int a;
  int b;
  int c = 42;
};

void
fn_B ()
{
  int i = 0;
  auto y = new B(1, 2);
  auto x = new B(++i, ++i);
  auto z = new B(1, { ++i });
  auto u = new B(1, f());
}

struct C
{
  int a;
  int b = 10;
};

void
fn_C ()
{
  int i = 0;
  auto y = new C(1);
  auto x = new C(++i);
  auto z = new C({ ++i });
  auto u = new C(f());
}
