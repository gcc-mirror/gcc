/* PR c++/8237 */
/* { dg-do compile } */
class A {
public:
  A() { }
};
 
class B {
public:
  B(A a) { }
  void form() { }
};
 
int main() {
   // This used to give a parse error.
   B(A()).form();
}
 
