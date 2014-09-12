// PR c++/63201
// { dg-do compile { target c++14 } }

template <class T>
struct Y
{
  template <class U> static U x;
};

template <class T>
template <class U>
U Y<T>::x = U();

template <>
template <class U>
U Y<int>::x = 42;

template <>
template <class U>
// odd diagnostic
U Y<float>::x<U> = 42; // { dg-error "partial specialization" }

template <>
template <>
int Y<float>::x<int> = 42; // { dg-bogus "non-member-template declaration" }

template <class T>
struct Z
{
  template <class U> struct ZZ
  {
    template <class V> static V x;
  };
};

template <class T>
template <class U>
template <class V>
V Z<T>::ZZ<U>::x = V();

template <>
template <>
template <class V>
V Z<int>::ZZ<int>::x = V();

template <>
template <class U>
struct Z<float>::ZZ
{
  template <class V> static V x;
};

template <>
template <class U>
template <class V>
V Z<float>::ZZ<U>::x = V();

template <>
template <>
template <>
int Z<float>::ZZ<int>::x<int> = 42; // { dg-bogus "non-member-template declaration" }

int main()
{
  int y = Y<int>::x<int>;
  int z = Z<float>::ZZ<int>::x<int>;
}
