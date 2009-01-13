// PR c++/35109

struct C;
void f() {
  struct A {
    friend struct B;
    friend struct C;
    void g()
    {
      B *b;			// { dg-error "not declared" }
      C* c;			// OK, finds ::C
    } 
  };
  C *c; // OK, finds ::C
  struct B {};
  B *b; // OK, now it isn't hidden
}
