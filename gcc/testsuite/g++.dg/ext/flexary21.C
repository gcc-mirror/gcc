// PR c++/72775
// { dg-do compile { target c++11 } }
// { dg-options -Wno-pedantic }

struct S {
  int i;
  char a[];
  S () : a("bob") {} // { dg-error "initializer for flexible array member" }
};

struct T {
  int i;
  char b[] = "bob";      // { dg-error "initializer for flexible array member" }
  T () {
    // the presence of this ctor definition elicits the error above
    // without it the flexible array initializer would be ignored
    // and so (unfortunately) not diagnosed
  }
  T (int) : b("bob") {}  // { dg-error "initializer for flexible array member" }
};
