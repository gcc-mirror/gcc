// PR c++/85710
// { dg-additional-options -Wmemset-elt-size }
// { dg-skip-if "requires hosted libstdc++ for cstring" { ! hostedlib } }

#include <cstring>

template <typename T> struct A { int a; };

void foo(A<int> (*ap)[2])
{
  std::memset (*ap, 0, 2);	// no warning because A<int> is incomplete
}

template <typename T>
class E
{
public:
  void Clear();
private:
  A<T> mA[2];
};

template<typename T>
void E<T>::Clear()
{
  std::memset(mA, 0, 2);	// { dg-warning -Wmemset-elt-size }
}

int main()
{
  E<int>().Clear();
}
