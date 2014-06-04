// PR c++/43453

struct A {
  char x[4]; 
  A() : x("bug") { };
};

char x [4] ("bug");

struct CA {
  const char cx[4]; 
  CA() : cx("bug") { };
};

const char cx [4] ("bug");

struct B {
  char y[4]; 
  B() : y("bu") { };
};

char y [4] ("bu");

struct C {
  char z[4]; 
  C() : z("bugs") { };  // { dg-error "too long" }
};

char z [4] ("bugs");    // { dg-error "too long" }

char k [] ("bug");

const char ck [] ("bug");
