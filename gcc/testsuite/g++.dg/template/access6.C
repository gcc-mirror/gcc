// { dg-do compile }
// Origin: Detlef Vollmann <dv@vollmann.ch>

// PR c++/8389
// Access control ICE for typename during instantiation and name mangling

template <class> class Base {
  protected:
    typedef int Type;
};
    
template <class T> struct Derived : public Base<T> {
  typedef typename Base<T>::Type Type;
  template <class Arg> void f(Type = Type()) {}
};
    
template void Derived<char>::f<int> (Type);
