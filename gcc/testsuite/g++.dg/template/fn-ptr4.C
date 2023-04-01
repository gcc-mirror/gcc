// { dg-do compile }

template<void (*P)()>
void wrap() {
  P(); // OK, despite A::g not being accessible from here.
}

struct A {
  static void f() {
    wrap<A::g>();
  }
private:
  static void g();
};
