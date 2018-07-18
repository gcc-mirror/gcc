// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool Nothrow_assignable() { return __has_nothrow_assign(T); }

template<typename T>
  concept bool Nothrow_constructible() { return __has_nothrow_constructor(T); }

template<typename T>
  concept bool Nothrow_copyable() { return __has_nothrow_copy(T); }

template<typename T>
  concept bool Trivially_assignable() { return __has_trivial_assign(T); }

template<typename T>
  concept bool Trivially_constructible() { return __has_trivial_constructor(T); }

template<typename T>
  concept bool Trivially_copyable() { return __has_trivial_copy(T); }

template<typename T>
  concept bool Trivially_destructible() { return __has_trivial_destructor(T); }

template<typename T>
  concept bool Dynamically_destructible() { return __has_virtual_destructor(T); }

template<typename T>
  concept bool Abstract() { return __is_abstract(T); }

template<typename T>
  concept bool Polymorphic() { return __is_polymorphic(T); }

template<typename T>
  concept bool Class() { return __is_class(T); }

template<typename T>
  concept bool Empty() { return __is_empty(T); }

template<typename T>
  concept bool Enum() { return __is_enum(T); }

template<typename T>
  concept bool Final() { return __is_final(T); }

template<typename T>
  concept bool Literal_type() { return __is_literal_type(T); }

template<typename T>
  concept bool Pod() { return __is_pod(T); }

template<typename T>
  concept bool Standard_layout() { return __is_standard_layout(T); }

template<typename T>
  concept bool Trivial() { return __is_trivial(T); }

template<typename T>
  concept bool Union() { return __is_union(T); }

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
  f1<void>(); // { dg-error "cannot call" }
  f2<void>(); // { dg-error "cannot call" }
  f3<void>(); // { dg-error "cannot call" }
  f4<void>(); // { dg-error "cannot call" }
  f5<void>(); // { dg-error "cannot call" }
  f6<void>(); // { dg-error "cannot call" }
  f7<void>(); // { dg-error "cannot call" }
  f8<void>(); // { dg-error "cannot call" }
  f9<void>(); // { dg-error "cannot call" }
  f10<void>(); // { dg-error "cannot call" }
  f11<void>(); // { dg-error "cannot call" }
  f12<void>(); // { dg-error "cannot call" }
  f13<void>(); // { dg-error "cannot call" }
  f14<void>(); // { dg-error "cannot call" }
  f15<void>(); // { dg-error "cannot call" }
  f16<void>(); // { dg-error "cannot call" }
  f17<void>(); // { dg-error "cannot call" }
}
