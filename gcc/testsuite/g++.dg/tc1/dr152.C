// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR152: explicit copy constructors 

namespace N1 {
  struct X {
    X();			// { dg-message "note" }
    explicit X(const X&);
  };
  void f(X);			// { dg-error "initializing" }
  int foo() 
  { 
    X x; 
    f(x);     // { dg-error "matching" "matching" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 14 }
  }
}

namespace N2 {
  template <class T>
  struct X {
    X();			// { dg-message "note" }
    explicit X(const X&);
  };

  template <class T>
  void f(T ) {}			// { dg-error "initializing" }
  
  template <class T>
  int foo() 
  { 
    X<T> x; 
    N2::f(x);   // { dg-error "matching" "matching" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 33 }
  }

  template int foo<float>();  // { dg-message "instantiated from here" }
}
