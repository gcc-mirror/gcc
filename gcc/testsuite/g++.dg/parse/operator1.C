/* PR c++/8982 */
/* { dg-do compile } */
namespace foo {
  template<class X>
  int operator- (X x);
}
 
int main() {
  using foo::operator-;  // syntax error under gcc 3.2
}

