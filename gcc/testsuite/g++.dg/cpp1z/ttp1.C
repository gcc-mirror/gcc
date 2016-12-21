// CWG 150: Matching of template template-arguments excludes compatible
// templates

// { dg-options -fnew-ttp-matching }

template<class T, class U = T> class B { /* ... */ };
#if __cpp_variadic_templates
template <class ... Types> class C { /* ... */ };
#endif
template<template<class> class P, class T> void f(P<T>);

int main()
{
  f(B<int>());
  f(B<int,float>());		// { dg-error "no match" }
#if __cpp_variadic_templates
  f(C<int>());
#endif
}
