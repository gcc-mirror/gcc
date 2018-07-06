// PR c++/84602 ICE
// { dg-additional-options "-fpermissive" }

struct X {
  union {
    class a; // { dg-warning "public non-static data member" }
  };
  a *b;
};
X::a *a;

struct Y {
  union {
    class a; // { dg-warning "public non-static data member" }
    int a;
  };
  class a *b;
};

class Y::a *y;

struct Z {
  union {
    // Force MEMBER_VEC creation
    int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
    class a; // { dg-warning "public non-static data member" }
    int a;
  };
  class a *b;
};

class Z::a *z;
