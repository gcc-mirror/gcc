// PR c++/13925

namespace N {
  template <class T> void f(T);

  namespace M {
    class A {
      friend void f<int>(int);
    };
  }

  template <class T> void f(T) {}
  template <> void f<int>(int )
  { 
    f<long>(0);
  }
}
