// { dg-do compile { target c++17 } }

template <class T>
struct A
{
  int i;
  A(...);
};

template <class T>
static A(T) -> A<T>;		// { dg-error "1:decl-specifier" }
