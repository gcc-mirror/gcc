// { dg-do run  }
// Check namespace aliases inside blocks
namespace A { 
  int i;
  void f(){
    i = 0;
  }
}

int g();

int main ()
{
  namespace B = A;
  B::i=42;
  B::f();
  using namespace B;
  f();
  // A::i is now 0, B::i is 1
  return g();
}

namespace B {
  int i = 1;
}

int g()
{
  namespace x = A;
  if (x::i)
  {
    namespace x = B;
    return x::i;
  }
  return x::i;
}
