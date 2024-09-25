// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept C1 = __is_class(T);

template<typename T>
  concept C2 = __is_class(T);

template<typename T>
  constexpr bool C3 = __is_class(T);


template<typename U>
  requires C1<U>
  void f1(U) { }

template<typename U>
  requires C2<U>
  void f2(U) { }

template<C3 T>  // { dg-error "not a type" }
  void f(T) { } // { dg-error "declared void|not declared" }

void foo()
{
  struct S { } s;
  f2(s);
  // f2(0);
}
