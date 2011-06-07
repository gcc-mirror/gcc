// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR166: Friend declarations of template-ids 

namespace N {
  template <class T> void f(T);
  void g();

  namespace M {
    class A {
      friend void f<int>(int); // N::f
      static int x;   // { dg-error "private" }
    };
    
    class B {
      template <class T> friend void f(T);  // M::f
      static int x;   // { dg-error "private" }
    };

    class C {
      friend void g(); // M::g
      static int x;   // { dg-error "private" }
    };

    template <class T> void f(T)  // will be instantiated as f<long>
    {
      M::A::x = 0;    // { dg-error "within this context" }
      M::B::x = 0;
    }
    template <> void f<int>(int)
    { M::A::x = 0; }      // { dg-error "within this context" }
    template <> void f<double>(double )
    { 
      M::B::x = 0; 
      M::f<long>(0);   // { dg-message "required" }
    }

    void g(void)
    { M::C::x = 0; }
  }

  template <class T> void f(T)  // will be instantiated as f<long>
  { 
    M::A::x = 0;       // { dg-error "within this context" }
    M::B::x = 0;       // { dg-error "within this context" }
  }

  template <> void f<int>(int )
  { 
    N::f<long>(0);        // { dg-message "required" }
    M::A::x = 0; 
    M::B::x = 0;       // { dg-error "within this context" }
  }

  template <> void f<char>(char )
  { M::A::x = 0; }      // { dg-error "within this context" }

  void g(void)
  { M::C::x = 0; }      // { dg-error "within this context" }
}
