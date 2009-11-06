// Test for helpful error message about missing typename.

template <class T> struct A { typedef T foo; typedef T bar; };
template <class T>
void f(T t)
{
  typedef A<T>::foo type;	// { dg-error "typename" }
  A<T>::bar b;			// { dg-error "typename" }
} // { dg-error "expected ';'" "" { target *-*-* } 8 }

template <class T> struct B
{
  void f()
  {
    A<T>::baz z;		// { dg-error "typename" }
  } // { dg-error "expected ';'" "" { target *-*-* } 15 }
};
