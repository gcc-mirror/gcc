// Build don't link:
class A {
public:
  void z();
  A(void) {}
private:
  A(const A &) { abort(); } // ERROR - 
  const A& operator =(const A &) { abort(); }
};

class B : public A {
public:
  B(void) {}
};

void f(B b) {
};

void g() {
  B h;
  f(h); // ERROR - 
}
