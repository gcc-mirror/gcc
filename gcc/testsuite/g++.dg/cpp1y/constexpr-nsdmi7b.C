// PR c++/94034
// { dg-do compile { target c++14 } }

struct A
{
  A() = default; A(const A&);
  A *p = this;
  int n = 2;
  int m = p->n++;
};

constexpr A
foo()
{
  return {};
}

constexpr A
bar()
{
  A a = foo();
  a.p->n = 5;
  return a; // { dg-error "non-.constexpr." "" { target c++20_down } }
}

constexpr int
baz()
{
  A b = foo();
  b.p->n = 10;
  A c = foo();
  if (c.p->n != 3 || c.p->m != 2)
    __builtin_abort();
  foo();
  return 0;
}

static_assert(baz() == 0, "");

constexpr int
quux()
{
  const A d = foo();
  d.p->n++; // { dg-error "const object" }
  return 0;
}

static_assert(quux() == 0, ""); // { dg-error "non-constant" }
