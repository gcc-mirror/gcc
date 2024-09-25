// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept C = __is_class(T);

C{T} void f1();	// { dg-error "expected" }

struct S1
{
  C{T} void f2(); // { dg-error "expected" }
  C{T} static void f3();  // { dg-error "expected" }
};
