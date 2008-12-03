// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR152: explicit copy constructors 

namespace N1 {
  struct X {
    X();			// { dg-message "candidate" }
    explicit X(const X&);
  };
  void f(X);
  int foo() 
  { 
    X x; 
    f(x);     // { dg-error "matching" "matching" }
	      // { dg-error "initializing" "initializing" { target *-*-* } 14 }
  }
}

namespace N2 {
  template <class T>
  struct X {
    X();			// { dg-message "candidate" }
    explicit X(const X&);
  };

  template <class T>
  void f(T ) {}
  
  template <class T>
  int foo() 
  { 
    X<T> x; 
    N2::f(x);   // { dg-error "matching" "matching" }
		// { dg-error "initializing " initializing" { target *-*-* } 33 }
  }

  template int foo<float>();  // { dg-message "instantiated from here" }
}
