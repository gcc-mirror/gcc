// { dg-do compile }
// { dg-options -std=c++98 }
// Origin: <drow at gcc dot gnu dot org>,
//         <giovannibajo at gcc dot gnu dot org>
// c++/13243: Template parameters of non integral or enumeration type can't be
//  used for integral constant expressions. ADDR_EXPR and INDIRECT_REF are
//  invalid too.

template <int T> class foo {};
template <int *T> class bar {};

template <int *PI>
void dep5(bar<PI> *);

template <int *PI>
void dep6(bar<PI+1> *); // { dg-error "" "integral or enumeration" }

template <int I>
void dep7(bar<I+1> *);		// { dg-error "16:could not convert template argument" }

template <int *PI>
void dep8(foo< *PI > *); // { dg-error "" "integral or enumeration" }

template <int PI[1]>
void dep9(foo< *PI > *); // { dg-error "" "integral or enumeration" }

template <int PI[1]>
void dep9a(foo< sizeof(*PI) > *);

template <int PI[1]>
void dep10(foo< PI[0] > *); // { dg-error "" "integral or enumeration" }

template <int I>
void dep11(foo< *&I > *); // { dg-error "" "constant expression" }

template <int I>
void dep12(foo< (&I)[4] > *); // { dg-error "" "constant expression" }

