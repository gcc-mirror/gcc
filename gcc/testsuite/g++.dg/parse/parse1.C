/* PR c++/50 */
/* { dg-do compile } */

namespace A {typedef int Z;}
int main(void)
{
  A::Z* z;
  z->A::Z::~Z();
}
