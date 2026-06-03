// PR c++/124794
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -Wno-error=missing-template-keyword" }

struct C{
  template <class T> void f(T);
  void g(int);
};

void (C::*p0)(int) = &template[:^^C::f:]<int>;
void (C::*p1)(int) = template[:^^C::f:]<int>; // { dg-error "cannot implicitly reference a class member" }
void (C::*p2)(int) = &template[:^^C::f:];
void (C::*p3)(int) = template[:^^C::f:]; // { dg-error "cannot implicitly reference a class member" }
void (C::*p4)(int) = &[:^^C::f:]<int>; // { dg-warning "keyword before dependent template name" }
void (C::*p5)(int) = [:^^C::f:]<int>; // { dg-error "cannot implicitly reference a class member" }
void (C::*p6)(int) = &[:^^C::f:]; // { dg-warning "keyword before dependent template name" }
void (C::*p7)(int) = [:^^C::f:]; // { dg-error "cannot implicitly reference a class member" }

template <auto r> void (C::*tp0)(int) = &template[:r:]<int>;
template <auto r> void (C::*tp1)(int) = template[:r:]<int>; // { dg-error "cannot implicitly reference a class member" }
template <auto r> void (C::*tp2)(int) = &template[:r:];
template <auto r> void (C::*tp3)(int) = template[:r:]; // { dg-error "cannot implicitly reference a class member" }
/* tp4 and tp5 intentionally omitted as they are not applicable in a template context */
template <auto r> void (C::*tp6)(int) = &[:r:];
template <auto r> void (C::*tp7)(int) = [:r:]; // { dg-error "cannot implicitly reference a class member" }
template <auto r> void (C::*tp0n)(int) = &template[:r:]<int>; // { dg-error "no matches converting function" }
template <auto r> void (C::*tp1n)(int) = template[:r:]<int>; // { dg-error "cannot implicitly reference a class member" }
template <auto r> void (C::*tp2n)(int) = &template[:r:]; // { dg-error "expected a reflection of a function template" }
template <auto r> void (C::*tp3n)(int) = template[:r:]; // { dg-error "cannot implicitly reference a class member" }
/* tp4u and tp5u intentionally omitted as they are not applicable in a template context */
template <auto r> void (C::*tp6n)(int) = &[:r:];
template <auto r> void (C::*tp7n)(int) = [:r:]; // { dg-error "cannot implicitly reference a class member" }

static_assert((
    tp0<^^C::f>,
    tp1<^^C::f>,
    tp2<^^C::f>,
    tp3<^^C::f>,
    tp6<^^C::f>,
    tp7<^^C::f>,
    tp0n<^^C::g>,
    tp1n<^^C::g>,
    tp2n<^^C::g>,
    tp3n<^^C::g>,
    tp6n<^^C::g>,
    tp7n<^^C::g>,
    true));

struct Base1{
  template<class T>
  constexpr T f(T x) {
    return x;
  }
};
struct Base2: Base1 {
  template<class T>
  constexpr T g(T x) {
    return x;
  }
};
struct Base3: Base1, Base2 {}; // { dg-warning "inaccessible" }

static_assert(Base1{}.[:^^Base1::f:](4) == 4); // { dg-warning "keyword before dependent template name" }
static_assert(Base1{}.[:^^Base1::f:]<int>(13) == 13); // { dg-warning "keyword before dependent template name" }
static_assert(Base3{}.[:^^Base2::g:](42) == 42); // { dg-warning "keyword before dependent template name" }
static_assert(Base3{}.[:^^Base2::g:]<int>(67) == 67); // { dg-warning "keyword before dependent template name" }
constexpr int invalid1 = Base3{}.[:^^Base2::f:]; // { dg-error "cannot resolve overloaded function" }
// { dg-warning "keyword before dependent template name" "" { target *-*-* } .-1 }
constexpr int invalid2 = Base3{}.[:^^Base1::f:]; // { dg-error "cannot resolve overloaded function" }
// { dg-warning "keyword before dependent template name" "" { target *-*-* } .-1 }
