// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

typedef int I;
typedef int I;

struct A {
  typedef int I;
  typedef int I;
};

template <class T>
struct S {
  typedef int I;
  typedef int I;
};

