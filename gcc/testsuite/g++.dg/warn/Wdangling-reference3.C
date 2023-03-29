// PR c++/106393
// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }

struct A {
   int ar[4];
   int& operator[](int i) { return ar[i]; }
};
const int &r = A()[2]; // { dg-warning "dangling reference" }

struct S {
  const S& self () { return *this; }
};
const S& s = S().self(); // { dg-warning "dangling reference" }

struct G {
  const G& g() { return *this; }
};

struct F {
  G& f();
};

const G& g = F().f().g(); // { dg-warning "dangling reference" }
