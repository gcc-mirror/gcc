// PR c++/93633
// { dg-do compile { target c++20 } }

struct A {
  constexpr A () : a (0) {}
  virtual int foo () { return 1 + a * 4; }
  int a;
};

struct B : A {
  constexpr B () : b (0) {}
  virtual int foo () { return 0 + b * 4; }	// { dg-message "declared here" }
  int b;
};

constexpr int
foo ()
{
  A *a = new B ();
  a->a = 4;
  int r = a->foo ();	// { dg-error "call to non-.constexpr. function" }
  delete a;
  return r;
}

constexpr auto a = foo ();
