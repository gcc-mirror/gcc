// { dg-do assemble  }

struct S {
  S(int);
  S(int); // { dg-error "" } already declared

  ~S();
  ~S(); // { dg-error "" } already declared
};
