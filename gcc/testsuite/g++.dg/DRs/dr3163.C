// CWG 3163

  struct A {};
  struct B : A {};
  B::A(A); // OK, declares a variable named `A` having type `A`
  B::B(A); // { dg-error "" } out-of-line non-defining constructor declaration
