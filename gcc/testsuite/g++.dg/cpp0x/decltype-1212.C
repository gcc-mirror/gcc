// Core 1212
// { dg-options -std=c++0x }

template <class T, class U> struct assert_same_type;
template <class T> struct assert_same_type<T,T> {};

int main()
{
  int i;
  assert_same_type<int&&,decltype(static_cast<int&&>(i))>();
}
