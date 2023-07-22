// PR c++/93633
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

struct A {
  constexpr A () : a (0) {}
  constexpr virtual int foo () { return 1 + a * 4; }
  int a;
};

struct B : A {
  constexpr B () : b (0) {}
  constexpr virtual int foo () { return 0 + b * 4; }
  int b;
};

constexpr int
foo ()
{
  A *a = new B ();  // { dg-message "allocated here" }
  a->a = 4;
  delete a;
  int r = a->foo ();  // { dg-error "constant expression" }
  return r;
}

constexpr auto a = foo ();  // { dg-message "in .constexpr. expansion" }
