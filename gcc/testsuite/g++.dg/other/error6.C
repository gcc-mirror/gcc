// { dg-do compile }
// Make sure we emit a decent error message when trying to mangle an
//  expression not supported by the C++ ABI due to a defect.

template <int N>
struct A {};

struct B
{
  static int foo(void);
};

template <class T>
A<sizeof(T::foo())> func(void);

int main()
{
  func<B>();  // { dg-error "sorry, unimplemented" }
}
