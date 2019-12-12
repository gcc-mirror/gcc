// PR c++/15946
// Test for helpful error message about missing typename.

template <class T> struct A
{
  struct B { };
  static B b;
};

template <class T>
void f(T t)
{
  typedef A<T>::foo type;	// { dg-error "typename" }
  A<T>::bar b;			// { dg-error "typename" "typename" }
} // { dg-error "expected ';'" "expected" { target *-*-* } .-1 }

// PR c++/36353
template <class T> struct B
{
  void f()
  {
    A<T>::baz z;		// { dg-error "typename" "typename" }
  } // { dg-error "expected ';'" "expected" { target *-*-* } .-1 }
};

// PR c++/40738
template <class T>
void g(const A<T>::type &t);	// { dg-error "typename" }

// PR c++/18451
template <class T> A<T>::B A<T>::b; // { dg-error "typename" "" { target c++17_down } }
