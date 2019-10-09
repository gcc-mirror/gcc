// { dg-do compile { target c++2a } }

template <class T>
struct A {
  template <class U>
    requires (sizeof(T) == 1)
      static void f(U);
  
  template <class U>
    requires (sizeof(T) == 2)
      static void f(U);
  
  void g()
  {
    f(42);
  }
};
