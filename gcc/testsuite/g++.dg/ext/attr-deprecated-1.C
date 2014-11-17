// PR c++/33911

template<typename T> struct __attribute__ ((deprecated)) S {};
S<int> s;			// { dg-warning "deprecated" }

template <template <class> class T> struct A { };
A<S> a;				// { dg-warning "deprecated" }

template <class T> void f() __attribute__ ((deprecated));

int main()
{
  f<int>();			// { dg-warning "deprecated" }
  void (*p)() = f<char>;	// { dg-warning "deprecated" }
}
