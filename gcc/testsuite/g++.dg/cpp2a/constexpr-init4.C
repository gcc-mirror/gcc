// PR c++/91353 - P1331R2: Allow trivial default init in constexpr contexts.
// { dg-do compile { target c++20 } }

// This bullet in [dcl.constexpr] is now gone:
//  - every non-static data member and base class sub-object shall be initialized

struct A {
  int i;
  constexpr A(int _i) { i = _i; }
};

struct B {
  int i;
  constexpr B() { }
};

// Anonymous members.
struct E {
  int a;
  union {
    char b;
    __extension__ struct {
      double c;
      long d;
    };  
    union {
      char e;
      void *f; 
    };  
  };  
  __extension__ struct {
    long long g;
    __extension__ struct {
      int h;
      double i;
    };  
    union {
      char *j; 
      E *k; 
    };  
  };  

  // Completely initialized.
  constexpr E(int(&)[1]) : a(), b(), g(), h(), i(), j() {}
  constexpr E(int(&)[3]) : a(), e(), g(), h(), i(), k() {}
  constexpr E(int(&)[7]) : a(), b(), g(), h(), i(), j() {}
  constexpr E(int(&)[8]) : a(), f(), g(), h(), i(), k() {}
  constexpr E(int(&)[9]) : a(), c(), d(), g(), h(), i(), k() {}

  // Missing d, i, j/k union init.
  constexpr E(int(&)[2]) : a(), c(), g(), h() {}

  // Missing h, j/k union init.
  constexpr E(int(&)[4]) : a(), c(), d(), g(), i() {}

  // Missing b/c/d/e/f union init.
  constexpr E(int(&)[5]) : a(), g(), h(), i(), k() {}

  // Missing a, b/c/d/e/f union, g/h/i/j/k struct init.
  constexpr E(int(&)[6]) {}
};
