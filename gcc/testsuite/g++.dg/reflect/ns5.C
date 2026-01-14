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

template<info R> void fn1 () { typename [:R:]::X x; }	// { dg-error "not usable in a splice scope" }
template<info R> void fn2 () { typename [:R:]::X x; }   // { dg-error "not usable in a splice scope" }
template<info R> void fn3 () { typename [:R:]::X x; }   // { dg-error "not usable in a splice scope" }
template<info R> void fn4 () { typename [:R:]::X x; }   // { dg-error "not usable in a splice scope" }
template<info R> void fn5 () { typename [:R:]::X x; }   // { dg-error "not usable in a splice scope" }
template<info R> void fn6 () { typename [:R:]::X x; }   // { dg-error "not usable in a splice scope" }
template<info R> void fn7 () { typename [:R:]::X x; }   // { dg-error "not usable in a splice scope" }
template<info R> void fn8 () { typename [:R:]::X x; }   // { dg-error "not usable in a splice scope" }
template<info R> void fn9 () { typename [:R:]::X x; }   // { dg-error "not usable in a splice scope" }
template<info R> void fn10 () { typename [:R:]::X x; }  // { dg-error "not usable in a splice scope" }
template<info R> void fn11 () { typename [:R:]::X x; }  // { dg-error "not usable in a splice scope" }

template void fn1<^^foo<int>>(); // { dg-message "required from here" }
template void fn2<^^foo>();	 // { dg-message "required from here" }
template void fn3<^^bar>();	 // { dg-message "required from here" }
template void fn4<^^vt<int>>();  // { dg-message "required from here" }
template void fn5<^^vt>();	 // { dg-message "required from here" }
template void fn6<^^C>();	 // { dg-message "required from here" }
template void fn7<^^i>();	 // { dg-message "required from here" }
template void fn8<^^X>();	 // { dg-message "required from here" }
template void fn9<^^S::si>();	 // { dg-message "required from here" }
template void fn10<^^D<int>::di>(); // { dg-message "required from here" }
template void fn11<^^Z>();	 // { dg-message "required from here" }
