namespace A { 
  int i;
  void f(){}
}

main ()
{
  namespace B = A;
  B::i=42;
  B::f();
  using namespace B;
  f();
}

namespace B {}

