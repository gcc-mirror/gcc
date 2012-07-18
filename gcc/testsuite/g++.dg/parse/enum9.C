// PR c++/53995

enum E1 { e };
void f(E1);

struct A {
  int i1,i2,i3,i4,i5,i6,i7,i8,i9,i10;
  void g();
  void h();
};

void A::g() { enum E2 { e }; }
void A::h() { f(e); }
