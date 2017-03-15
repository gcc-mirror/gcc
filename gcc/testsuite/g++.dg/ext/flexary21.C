// PR c++/72775
// { dg-do compile { target c++11 } }
// { dg-options -Wno-pedantic }

struct S {
  int i;
  char a[];
  S () : a("bob") {} // { dg-error "member initializer for flexible array member" }
};

struct T {
  int i;
  char a[] = "bob";
  T () : a("bob") {} // { dg-error "member initializer for flexible array member" }
};
