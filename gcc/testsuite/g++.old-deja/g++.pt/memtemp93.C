// { dg-do link  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <int n> struct A { 
  template <class T> A (T t);
  template <class T> int f(T t) const; 
}; 

template <> template<class T> int A<1>::f(T t) const {return 1;} 
template <> template<class T> A<1>::A (T t) {}

int main() { 
  A<1> a (3);
  a.f(1);
  return 0; 
}
