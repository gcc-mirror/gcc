// PR c++/59238
// { dg-do compile { target c++11 } }

struct A { ~A () = delete; };
A *pa{new A{}};
A *pa2{new A[2]{}};

class B { ~B () = default; };
B *pb{new B{}};

struct E {
  ~E () = delete;
private:
  int x;
};
E *pe{new E{}};

class C { ~C (); };
C *pc{new C{}};

class D { ~D () {} };
D *pd{new D{}};

struct F {
  F () = default;
  ~F () = delete;
};
F *pf{new F{}};

struct G {
  G () = default;
  ~G () = delete;
private:
  int x;
};
G *pg{new G{}};
