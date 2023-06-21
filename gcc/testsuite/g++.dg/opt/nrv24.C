// PR c++/58050
// { dg-do link }

struct B {
  B() { }
  B(const B&); // not defined, link error on unnecessary copy
  ~B() { }
};
struct A {
  static B make() { return B(); }
} a;
A *ap() { return &a; }
int main () {
  {B b = A::make();}
  {B B = a.make();}
  {B b = ap()->make();}
  {B b = A().make();}
}
