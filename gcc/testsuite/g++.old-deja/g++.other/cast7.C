// { dg-do run  }
// Test that we can add cv-quals in a static cast to a pointer-to-base type.

struct A { int i; };
struct B : public A {};

int main()
{
  int B::* bp = &B::i;
  const int A::* ap = static_cast<const int A::*>(bp);
  return ap != bp;
}
