// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// From [temp.names].

using size_t = decltype(sizeof(int));
using info = decltype(^^void);

struct X {
  template<size_t> X* alloc();
  template<size_t> static X* adjust();
};
template<class T> void f(T* p) {
  T* p1 = p->alloc<200>();              // { dg-error "expected" }
  // { dg-warning "expected .template. keyword before dependent template name" "" { target *-*-* } .-1 }
  T* p2 = p->template alloc<200>();     // OK, < starts template argument list
  T::adjust<100>();                     // { dg-error "expected" }
  // { dg-warning "expected .template. keyword before dependent template name" "" { target *-*-* } .-1 }
  T::template adjust<100>();            // OK, < starts template argument list

  static constexpr info r = ^^T::adjust;
  T* p3 = [:r:]<200>();                 // { dg-error "expected" }
  T* p4 = template [:r:]<200>();        // OK, < starts template argument list
}
