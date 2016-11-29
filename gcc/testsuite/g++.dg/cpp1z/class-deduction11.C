// { dg-options -std=c++1z }

template <class T>
struct A
{
  int i;
  A(...);
};

template <class T>
static A(T) -> A<T>;		// { dg-error "1:decl-specifier" }
