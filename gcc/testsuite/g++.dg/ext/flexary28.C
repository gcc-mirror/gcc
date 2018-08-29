// PR c++/83588 - struct with two flexible arrays causes an internal compiler
// error
// { dg-do compile }
// { dg-options "-Wno-pedantic" }

struct A {
  int i;
  int a[];          // { dg-error "flexible array member .A::a. not at end of .struct A." }
  int b[];
};

struct B {
  int i;
  int a[];          // { dg-error "flexible array member .B::a. not at end of .struct B." }
  int j;
  int b[][2];
};

struct C {
  int i;
  struct {
    int a[];        // { dg-error "flexible array member .C::<unnamed struct>::a. not at end of .struct C." }
  };
  int b[];
};

struct D {
  int i;
  struct {
    int a[];        // { dg-error "flexible array member .D::<unnamed struct>::a. not at end of .struct D." }
  } b[];
  int c[];
};

struct E {
  int i;
  int a[0];
  int b[];          // { dg-error "flexible array member .E::b. not at end of .struct E." }
  int d[];
};

struct F {
  int i;
  int a[];          // { dg-error "flexible array member .F::a. not at end of .struct F." }
  int b[], c[], d[];
};
