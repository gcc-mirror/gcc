// PR c++/91353 - P1331R2: Allow trivial default init in constexpr contexts.
// { dg-do compile { target c++20 } }

/* We used to get the "constexpr constructor for union S::<unnamed union>
   must initialize exactly one non-static data member" error, but not anymore
   in C++20.  */

struct S {
  union {
    int i;
    double d;
  };
  constexpr S() { }
};

union U {
  int a;
  constexpr U() { }
};

struct W {
  union {
    int a;
  };
  constexpr W() { }
};
