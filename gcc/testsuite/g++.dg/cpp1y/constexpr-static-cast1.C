// { dg-do compile { target c++14 } }

struct A { int a; };
struct B : A { int b; };
struct C : A { int c; };
struct D { int d = 42; };
struct E { int e = 0; };
struct F : E, D { };
struct G : F { };
struct H { G b; };

constexpr bool
foo (bool x)
{
  A a = {};
  if (x)
    {
      auto c = static_cast <B *> (&a);	// { dg-error "'A' operand is not a base class subobject of a 'B' object" }
    }
  return true;
}

constexpr bool
bar ()
{
  A a = {};
  B b = {};
  auto c = static_cast <A *> (&a);
  auto d = static_cast <A *> (&b);
  auto e = static_cast <B *> (&b);
  auto f = static_cast <B *> (d);
  auto g = static_cast <A *> (nullptr);
  auto h = static_cast <B *> (nullptr);
  auto i = static_cast <B *> (g);
  auto j = static_cast <A *> (h);
  return true;
}

constexpr bool
baz (bool x)
{
  C a = {};
  auto b = static_cast <A *> (&a);
  if (x)
    {
      auto c = static_cast <B *> (b);	// { dg-error "'A' operand \\\(of dynamic type 'C'\\\) is not a base class subobject of a 'B' object" }
    }
  return true;
}

#if __cpp_constexpr_dynamic_alloc >= 201907
constexpr bool
qux ()
{
  A *a = new B {};
  auto b = static_cast <B *> (a);
  auto c = static_cast <C *> (a);	// { dg-error "operand \\\(of dynamic type 'B'\\\) is not a base class subobject of a 'C' object" "" { target c++20 } }
  delete a;
  return true;
}

constexpr bool
corge ()
{
  A *a = new B[2] {};
  auto b = static_cast <B *> (a);
  auto c = static_cast <C *> (a);	// { dg-error "operand \\\(of dynamic type 'B'\\\) is not a base class subobject of a 'C' object" "" { target c++20 } }
  delete[] a;
  return true;
}
#endif

constexpr int
fred ()
{
  H h;
  auto a = static_cast <D *> (&h.b);
  auto b = static_cast <G *> (a);
  return b->d;
}

static_assert (foo (false), "");
constexpr bool a = foo (true);
static_assert (bar (), "");
static_assert (baz (false), "");
constexpr bool b = baz (true);
#if __cpp_constexpr_dynamic_alloc >= 201907
constexpr bool c = qux ();
constexpr bool d = corge ();
#endif
static_assert(fred () == 42, "");
