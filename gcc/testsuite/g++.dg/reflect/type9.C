// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^void);

template<typename>
void foo () { }
void bar () { }
namespace N { }
namespace M = N;
template<typename T>
T vt{};
template<typename T>
concept C = true;
static int i;
enum E { X };
struct S { static int si; };
template<typename T>
struct D { static T di; };
template<typename T>
using Z = D<T>;

template<info R> void fn1 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }
template<info R> void fn2 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }
template<info R> void fn3 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }
template<info R> void fn4 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }
template<info R> void fn5 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }
template<info R> void fn6 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }
template<info R> void fn7 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }
template<info R> void fn8 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }
template<info R> void fn9 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }
template<info R> void fn10 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }
template<info R> void fn11 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }
template<info R> void fn12 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }
template<info R> void fn13 () { int n = typename [:R:](42); } // { dg-error "not usable in a splice type" }

template void fn1<^^foo<int>>();  // { dg-message "required from here" }
template void fn2<^^foo>();	  // { dg-message "required from here" }
template void fn3<^^N>();	  // { dg-message "required from here" }
template void fn4<^^M>();	  // { dg-message "required from here" }
template void fn5<^^bar>();	  // { dg-message "required from here" }
template void fn6<^^vt<int>>();	  // { dg-message "required from here" }
template void fn7<^^vt>();	  // { dg-message "required from here" }
template void fn8<^^C>();	  // { dg-message "required from here" }
template void fn9<^^i>();	  // { dg-message "required from here" }
template void fn10<^^X>();	  // { dg-message "required from here" }
template void fn11<^^S::si>();	  // { dg-message "required from here" }
template void fn12<^^D<int>::di>(); // { dg-message "required from here" }
template void fn13<^^Z>();	  // { dg-message "required from here" }
