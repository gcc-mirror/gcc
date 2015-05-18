// DR 1391
// { dg-do compile { target c++11 } }

template<class T>
struct A {
  typename T::N n;
};

template<class T>
struct B { };

template <class T, class... U>
typename A<T>::value_t bar(int, T, U...);

template <class T>
T bar(T, T);

void baz()
{
  B<char> b;
  bar(b, b);
}
