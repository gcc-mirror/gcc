// { dg-do assemble  }
namespace A{
  namespace B{int i;}
  using namespace B;
}

namespace C=A;

void f(){
  C::i = 1;
}
