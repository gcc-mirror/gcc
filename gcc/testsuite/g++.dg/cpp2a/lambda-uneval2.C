// { dg-do compile { target c++2a } }

// ill-formed, no diagnostic required: the two expressions are
// functionally equivalent but not equivalent
template <int N> void foo(const char (&s)[([]{}, N)]);
template <int N> void foo(const char (&s)[([]{}, N)]);

// two different declarations because the non-dependent portions are not
// considered equivalent
template <class T> void spam(decltype([]{}) (*s)[sizeof(T)]);
template <class T> void spam(decltype([]{}) (*s)[sizeof(T)]);

template <class T>
using A = decltype([] { });
// A<int> and A<char> refer to different closure types

template <class T>
auto f(T) -> decltype([]() { T::invalid; } ()); // { dg-error "invalid" }
void f(...);

template <class T, unsigned = sizeof([]() { T::invalid; })> // { dg-error "invalid" }
void g(T);
void g(...);

template <class T>
auto h(T) -> decltype([x = T::invalid]() { });
void h(...);

template <class T>
auto i(T) -> decltype([]() -> typename T::invalid { });
void i(...);

template <class T>
auto j(T t) -> decltype([](auto x) -> decltype(x.invalid) { } (t));
void j(...);

template <class,class> struct different {};
template <class T> struct different<T,T> { typename T::invalid t; };

template <class,class> struct same;
template <class T> struct same<T,T> {};

int main()
{
  foo<1>("");	 // { dg-error "ambiguous" }
  spam<char>(nullptr);		// { dg-error "ambiguous" }
  different<A<int>,A<char>>();
  same<A<int>,A<int>>();
  f(0); // error: invalid expression not part of the immediate context
  g(0); // error: invalid expression not part of the immediate context
  h(0); // error: invalid expression not part of the immediate context
  i(0); // error: invalid expression not part of the immediate context
  j(0); // deduction fails on #1, calls #2
}
