/* PR c++/3009 */
/* { dg-do run } */
// According to 14.6.2.4 of C++ Standard:
// "If a base class is a dependent type, a member of that
// class cannot hide a name declared within a template, or a
// name from the template's enclosing scopes."
 
class B {
public:
  int foo() { return 1; }
};
 
int foo() { return 0; }
 
template <class T> class C : public T {
public:
  int caller() { return foo(); } // This must be ::foo, not B::foo.
};

int main() {
  C<B> c;
  return c.caller(); // Returns 1 if we got the wrong one.
}
