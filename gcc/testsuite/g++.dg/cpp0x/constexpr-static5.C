// { dg-options -std=c++0x }

template <class T>
struct A
{
  constexpr static T t = T();	// { dg-error "literal" }
};
template <class T>
constexpr T A<T>::t;

struct B
{
  ~B();
};

B b = A<B>::t;

