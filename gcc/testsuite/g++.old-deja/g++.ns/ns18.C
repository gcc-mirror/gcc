// { dg-do assemble  }
// Origin: Theo Papadopoulo <Theodore.Papadopoulo@sophia.inria.fr>

namespace Bname {
  class B;
}

template <class T>
class A {
  friend class Bname::B;
  static const int a = 1;
public:
  A() { }
};

namespace Bname {
  class B {
  	int a;
  public:
  	template<class T>
  	B(const T&):a(T::a) { }
  };
}

int
main()
{
  A<int> a;
  Bname::B b(a);
}
