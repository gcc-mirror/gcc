// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR142: Injection-related errors in access example 

class B {                 // { dg-error "inaccessible" }
public:
  int mi;                 // { dg-error "inaccessible" }
  static int si;          // { dg-error "inaccessible" }
};

class D: private B {
};

class DD: public D {
  void f();
};

void DD::f() {
  mi = 3;          // { dg-error "within this context" "" }
  si = 3;          // { dg-error "within this context" "" }
  ::B b;
  b.mi = 3;
  b.si = 3;
  ::B::si = 3;
  ::B* bp1 = this;        // { dg-error "inaccessible base" "" }
  ::B* bp2 = (::B*)this;
  bp2->mi = 3;


  B b2;                   // { dg-error "within this context" "" }
  B::si = 3;              // { dg-error "within this context" "" }
}
