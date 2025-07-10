// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

struct A {
  explicit constexpr A (int x) noexcept : a (x) {}
  constexpr virtual int foo () const noexcept { return a; }
  constexpr virtual ~A () {}
  int a;
};
struct B : public A {
  explicit constexpr B (int x) noexcept : A (x) {}
  constexpr int foo () const noexcept override { return a | 0x10; }
};
struct C : public A {
  explicit constexpr C (int x) noexcept : A (x) {}
};
struct D : public A {
  explicit constexpr D (int x) noexcept : A (x) {}
};
struct E {
  constexpr E () noexcept : e (0) {}
  explicit constexpr E (int x) noexcept : e (x) {}
  int e;
};
struct F : public E, public B {
  explicit constexpr F (int x) noexcept : B (x) {}
};
struct G : public E, public C {
  explicit constexpr G (int x) noexcept : C (x) {}
};
struct H : public E, public D {
  explicit constexpr H (int x) noexcept : D (x) {}
};

consteval int
bar (void (*fn) ())
{
  try
    {
      fn ();
    }
  catch (C &a)
    {
      return a.foo () | 0x20;
    }
  catch (const C &b)		// { dg-warning "exception of type 'C' will be caught by earlier handler" }
    {
      return b.foo () | 0x60;
    }
  catch (A &c)
    {
      return c.foo ();
    }
  catch (const A &d)		// { dg-warning "exception of type 'A' will be caught by earlier handler" }
    {
      return d.foo () | 0x40;
    }
  return -1;
}

static_assert (bar ([] { throw A { 1 }; }) == 1);
static_assert (bar ([] { throw B { 2 }; }) == 0x12);
static_assert (bar ([] { throw C { 3 }; }) == 0x23);
static_assert (bar ([] { throw D { 4 }; }) == 4);
constexpr int a = bar ([] { throw E { 5 }; });	// { dg-error "uncaught exception 'E\\\{5\\\}'" }
static_assert (bar ([] { throw F { 6 }; }) == 0x16);
static_assert (bar ([] { throw G { 7 }; }) == 0x27);
static_assert (bar ([] { throw H { 8 }; }) == 8);
