// { dg-do run  }
// Test to make sure g++ can handle target types that aren't identical
// with pointers to members.

struct A { int i; };
struct B : public A { };

int main ()
{
  int A::*p = &A::i;
  const int B::*q = &A::i;
  return p != q;
}
