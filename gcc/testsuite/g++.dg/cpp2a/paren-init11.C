// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++2a } }

// Test ill-formed code.

// If k is greater than the size of the array, the program is ill-formed.
int a1[2](1, 2, 3); // { dg-error "too many initializers" }
int a2[](); // { dg-error "array of functions" }
int a3[](()); // { dg-error "expected primary-expression" }
int a4[]("raccoon"); // { dg-error "invalid conversion" }

struct S {
  int i;
  int j;
};

S s1(1, 1, 1); // { dg-error "too many initializers" }

union U2 {
  int a;
  float b;
};

// [dcl.init.aggr]/19:
// When a union is initialized with an initializer list, there shall not be
// more than one explicitly initialized element.
U2 u4 = U2(1, 2); // { dg-error "too many initializers" }

// Test there is no brace elision.

int a[2][2](1, 2, 3, 4); // { dg-error "too many initializers|array must be initialized with a brace-enclosed initializer" }

// private/protected/virtual base class -> not an aggregate.
struct B { };
struct D : private B {
  int i;
  int j;
};

D d({}, 1, 2); // { dg-error "no matching function" }

// Private non-static data member -> not an aggregate.
struct P {
  int i;
private:
  int j;
};

P p(1, 2); // { dg-error "no matching function" }

// User-declared constructor -> not an aggregate.
struct U {
  U() {}
  int i;
  int j;  
};

U u(1, 2); // { dg-error "no matching function" }

// virtual member function -> not an aggregate.
struct V {
  int i;
  int j;
  virtual int foo(int);
};

V v(1, 2); // { dg-error "no matching function" }

struct nonaggr {
  int i;
  int j;
private:
  int x;
};

struct F {
  nonaggr n;
  F() : n(1) { } // { dg-error "no matching function" }
};

struct G {
  char a[4];
};

struct H {
  G g;
  H() : g("oaks") { } // { dg-error "initializer-string" }
};
