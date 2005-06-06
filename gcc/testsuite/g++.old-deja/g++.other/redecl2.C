// { dg-do assemble  }

struct S {
  S(int); // { dg-error "with" }
  S(int); // { dg-error "overloaded" } already declared

  ~S();// { dg-error "with" }
  ~S(); // { dg-error "overloaded" } already declared
};
