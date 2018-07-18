// PR c++/50308

void A( int ) __attribute__((deprecated));

namespace B {

  struct C {};

  void A(C) {}

}

int main ()
{
  B::C x;

  // ADL correctly identifies the non-deprecated B::A, but a warning about the
  // global A is generated anyway
  A( x );			// { dg-bogus "deprecated" }
}
