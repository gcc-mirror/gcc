// PR c++/19808
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized -Winit-self" }

int
foo (int *p)
{
  *p = 42;
  return 5;
}

struct S {
  int x;
  int y;
  int z;
  S() : x(foo (&y)), z(y) { } // { dg-bogus "uninitialized" }
};

struct T {
  int x;
  int y;
  int z;
  T() : x(({ y = 30; 42; })), z(y) { } // { dg-bogus "uninitialized" }
};

struct A {
  int x, y, z;
  int f () { y = 1; return 2; }
  A ():
    x (f ()),
    z (y) // { dg-bogus "uninitialized" }
  { }
};
