// { dg-do assemble  }

struct S {
  S(int); // { dg-message "previous" }
  S(int); // { dg-error "overloaded" } already declared

  ~S();// { dg-message "previous" }
  ~S(); // { dg-error "overloaded" } already declared
};
