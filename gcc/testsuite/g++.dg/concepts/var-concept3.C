// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool C1 = __is_class(T);

template<typename T>
  concept bool C2() { return __is_class(T); }

template<typename T>
  constexpr bool C3 = __is_class(T);


template<typename U>
  requires C1<U>() // { dg-error "cannot be used as a function" }
  void f1(U) { }

template<typename U>
  requires C2<U> // { dg-error "must be called" }
  void f2(U) { }

template<C3 T>  // { dg-error "not a type" }
  void f(T) { } // { dg-error "declared void|not declared" }

void foo()
{
  struct S { } s;
  f2(s);
  // f2(0);
}
