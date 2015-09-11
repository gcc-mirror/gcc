// { dg-do compile { target c++11 } }

template <class T, class = typename T::I> void f(T) {}
template <class T, class = typename T::I> void g(T) {}
template <class T, class = typename T::I> void h(T) {}
template <class T, class = typename T::I> void i(T) {}
template <class T, class = typename T::I> void j(T) {} // { dg-error "this context" }

class A
{
  typedef int I;		// { dg-message "private" }
  template <class T, class> friend void f(T);
  friend void g<A,I>(A);
  friend void h<A>(A);
  friend void i<>(A);
};

int main()
{
  A a;
  f(a);
  g(a);
  h(a);
  i(a);
  j(a);				// { dg-error "no match" }
}
