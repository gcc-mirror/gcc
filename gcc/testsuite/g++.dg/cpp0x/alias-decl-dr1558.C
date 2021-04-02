// DR 1558 still applies when using void_t as a template-argument.
// { dg-do compile { target c++11 } }
// { dg-additional-options "--param hash-table-verification-limit=10000" }

template<typename...> using void_t = void;
template<class T> struct A { };
struct B { typedef int foo; };
template<typename T> A<void_t<typename T::foo>> f(); // { dg-error "int" }
template<typename T> A<void> g();
int main()
{
  f<B>();
  g<int>();
  f<int>(); // { dg-error "no match" }
}
