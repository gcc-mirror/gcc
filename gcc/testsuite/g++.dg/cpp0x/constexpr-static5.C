// { dg-options -std=c++0x }

template <class T>
struct A
{
  constexpr static T t;
};
template <class T>
constexpr T A<T>::t = T();	// { dg-error "not literal" }

struct B
{
  ~B();
};

B b = A<B>::t;

