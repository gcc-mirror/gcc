// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {
  template <class U>
  struct I { 
    typedef U X;

    X f();
  };
};


template <class T>
template <class U>
typename S<T>::I<U>::X S<T>::I<U>::f() {}
