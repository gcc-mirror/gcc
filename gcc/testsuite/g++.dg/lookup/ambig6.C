// PR c++/103177

struct B1 {
private:
  static int foo();
};

struct B2 {
  int foo();
};

struct D : B1, B2 { };

void f() {
  D d;
  d.foo(); // { dg-error "ambiguous" }
           // { dg-bogus "private" "" { target *-*-* } .-1 }
}
