// { dg-do assemble  }
// { dg-options "" }
// Origin: Mark Mitchell <mark@codesourcery.com>

void X();

template <class T>
struct J {
  typedef T X;
};

template <class T>
struct S {
  typedef T X;

  struct I : public J<X> {
    static X* f();
  };
};

S<int> si;
