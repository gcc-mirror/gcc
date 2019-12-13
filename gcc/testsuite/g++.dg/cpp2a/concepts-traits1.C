// { dg-options "-std=c++2a" }

template<typename T>
  concept Nothrow_assignable = __has_nothrow_assign(T);

template<typename T>
  concept Nothrow_constructible = __has_nothrow_constructor(T);

template<typename T>
  concept Nothrow_copyable = __has_nothrow_copy(T);

template<typename T>
  concept Trivially_assignable = __has_trivial_assign(T);

template<typename T>
  concept Trivially_constructible = __has_trivial_constructor(T);

template<typename T>
  concept Trivially_copyable = __has_trivial_copy(T);

template<typename T>
  concept Trivially_destructible = __has_trivial_destructor(T);

template<typename T>
  concept Dynamically_destructible = __has_virtual_destructor(T);

template<typename T>
  concept Abstract = __is_abstract(T);

template<typename T>
  concept Polymorphic = __is_polymorphic(T);

template<typename T>
  concept Class = __is_class(T);

template<typename T>
  concept Empty = __is_empty(T);

template<typename T>
  concept Enum = __is_enum(T);

template<typename T>
  concept Final = __is_final(T);

template<typename T>
  concept Literal_type = __is_literal_type(T);

template<typename T>
  concept Pod = __is_pod(T);

template<typename T>
  concept Standard_layout = __is_standard_layout(T);

template<typename T>
  concept Trivial = __is_trivial(T);

template<typename T>
  concept Union = __is_union(T);

template<Nothrow_assignable T> void f1() { }
template<Nothrow_copyable T> void f2() { }
template<Nothrow_constructible T> void f3() { }
template<Trivially_assignable T> void f4() { }
template<Trivially_copyable T> void f5() { }
template<Trivially_constructible T> void f6() { }
template<Trivially_destructible T> void f7() { }
template<Dynamically_destructible T> void f8() { }
template<Class T> void f9() { }
template<Empty T> void f10() { }
template<Standard_layout T> void f11() { }
template<Pod T> void f12() { }
template<Trivial T> void f13() { }
template<Polymorphic T> void f14() { }
template<Abstract T> void f15() { }
template<Final T> void f16() { }
template<Union T> void f17() { }
template<Enum T> void f18() { }

int main() {
  f1<void>(); // { dg-error "" }
  f2<void>(); // { dg-error "" }
  f3<void>(); // { dg-error "" }
  f4<void>(); // { dg-error "" }
  f5<void>(); // { dg-error "" }
  f6<void>(); // { dg-error "" }
  f7<void>(); // { dg-error "" }
  f8<void>(); // { dg-error "" }
  f9<void>(); // { dg-error "" }
  f10<void>(); // { dg-error "" }
  f11<void>(); // { dg-error "" }
  f12<void>(); // { dg-error "" }
  f13<void>(); // { dg-error "" }
  f14<void>(); // { dg-error "" }
  f15<void>(); // { dg-error "" }
  f16<void>(); // { dg-error "" }
  f17<void>(); // { dg-error "" }
}
