namespace A{
  struct X{};
  void f(X&);
  extern int i;
  namespace a_very_long_namespace_name{
    int k;
  }
}

namespace B = A;
namespace B = A;
namespace B = B;

namespace avl = A::a_very_long_namespace_name;

void B::f(A::X& x)
{
  B::f(x);
  f(x);
  avl::k = 1;
}

int B::i = 0;

int main()
{
  B::X x;
  if (B::i)
    A::f(x);
}
