// PR c++/85710
// { dg-additional-options -Wmemset-elt-size }

#include <cstring>

template <typename T> struct A { int a; };

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
  std::memset(mA, 0, 2);
}
