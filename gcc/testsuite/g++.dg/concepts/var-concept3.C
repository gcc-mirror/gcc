// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool C1 = __is_class(T);

template<typename T>
  concept bool C2() { return __is_class(T); }

template<typename T>
  constexpr bool C3 = __is_class(T);


template<typename U>
  requires C1<U>() // { dg-error "" }
  void f1(U) { }

template<typename U>
  requires C2<U> // { dg-error "invalid reference" }
  void f2(U) { }

template<C3 T>  // { dg-error "not a type" }
  void f(T) { } // { dg-error "" }
