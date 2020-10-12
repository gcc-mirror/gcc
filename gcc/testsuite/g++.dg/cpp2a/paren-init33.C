// PR c++/92812
// { dg-do run { target c++20 } }
// { dg-options "-Wall -Wextra" }
// Initializing arrays in a member init list using ()-init, valid cases.

#define assert(X) do { if (!(X)) __builtin_abort(); } while(0)

struct S { int x, y; };
struct N { int x, y; N(int, int); };

static S s{10, 11};

struct A {
  S s1;
  S s2;
  S a1[2];
  S a2[2];
  S a3[2];
  S a4[2];
  S a5[2];
  S a6[2];
  A() : s1(1, 2),
	s2(1), // { dg-warning "missing initializer for member" }
	a1({1}), // { dg-warning "missing initializer for member" }
	a2({1, 2}),
	a3({}, {}),
	a4(),
	a5(s),
	a6(s, s)
    { }
};

struct C {
  int a1[2];
  int a2[2];
  int a3[2];
  int a4[2];
  int a5[2];
  C() : a1(1),
	a2(1, 2),
	a3({1}),
	a4({}, {}),
	a5()
    { }
};

struct D {
  N n;
  // Not an aggregate, should work pre-C++20 too.
  D() : n(1, 2) { }
};

struct E {
  char a1[4];
  char a2[4];
  E() : a1("ab"),
	a2("abc")
    { }
};

// Compound literal.
struct F {
  F ();
  S m[1];
};

F::F () : m(__extension__(S[1]) { 1, 2 })
{
}

struct B { int i; };
struct Der : B { };
Der d;
struct G {
  B b1[1];
  B b2[2];
  G(): b1(d),
       b2(d, d)
    { }
};

// Variation of c++/93790.
struct Empty { };
struct Empty_refwrap {
  Empty& r;
  Empty_refwrap(Empty &e) : r(e) { }
  operator Empty&() { return r; }
};

Empty empty;
Empty_refwrap empty_refwrap(empty);

struct H {
  Empty &e;
  // Turning this into {empty_refwrap} would break things.
  H() : e(empty_refwrap) { }
};

int
main ()
{
  A a;
  assert (a.s1.x == 1 && a.s1.y == 2);
  assert (a.s2.x == 1 && a.s2.y == 0);
  assert (a.a1[0].x == 1 && a.a1[0].y == 0
	  && a.a1[1].x == 0 && a.a1[1].y == 0);
  assert (a.a2[0].x == 1 && a.a2[0].y == 2
	  && a.a2[1].x == 0 && a.a2[1].y == 0);
  assert (a.a3[0].x == 0 && a.a3[0].y == 0
	  && a.a3[1].x == 0 && a.a3[1].y == 0);
  assert (a.a4[0].x == 0 && a.a4[0].y == 0
	  && a.a4[1].x == 0 && a.a4[1].y == 0);
  assert (a.a5[0].x == 10 && a.a5[0].y == 11
	  && a.a5[1].x == 0 && a.a5[1].y == 0);
  assert (a.a6[0].x == 10 && a.a6[0].y == 11
	  && a.a6[1].x == 10 && a.a6[1].y == 11);

  C c;
  assert (c.a1[0] == 1 && c.a1[1] == 0);
  assert (c.a2[0] == 1 && c.a2[1] == 2);
  assert (c.a3[0] == 1 && c.a3[1] == 0);
  assert (c.a4[0] == 0 && c.a4[1] == 0);
  assert (c.a5[0] == 0 && c.a5[1] == 0);

  E e;
  assert (__builtin_strcmp (e.a1, "ab") == 0
	  && __builtin_strcmp (e.a2, "abc") == 0);
}
