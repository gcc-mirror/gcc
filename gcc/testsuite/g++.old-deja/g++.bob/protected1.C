// Build don't link: 
class A {
public:
  int i;
  A(int j) : i(j){}
};

class B : protected A {
public:
  B(int j) : A(j){}
  void f(){
    A k(*this);
  }
};

class C : protected B {
public:
  C(int j) : B(j){}
  void f();

  void g(){
    A k(i); 
  }
};


class D : public C {
public:
   D(int w) : C(i) {}
   void j() { A k(*this); }
   void h() { i=3; }
};

void C::f() {
   A k(*this);
}

B b(3);
int
main() {
 A *z = &b; // ERROR - 
}
