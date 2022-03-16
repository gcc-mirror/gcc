/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

template<typename T> struct __attribute__ ((unavailable)) S {};
S<int> s;			// { dg-error "unavailable" }

template <template <class> class T> struct A { };
A<S> a;				// { dg-error "unavailable" }

template <class T> void f() __attribute__ ((unavailable));

int main()
{
  f<int>();			// { dg-error "unavailable" }
  void (*p)() = f<char>;	// { dg-error "unavailable" }
}
