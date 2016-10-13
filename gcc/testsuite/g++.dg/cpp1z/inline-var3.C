// { dg-do compile }
// { dg-options "-g0" }
// Verify that inline variables and static data members that aren't odr-used
// aren't emitted into assembly even at -O0.
// { dg-final { scan-assembler-not "inlvarvariable" } }

inline int inlvarvariable1 = 1;				// { dg-warning "inline variables are only available with" "" { target c++14_down } }
const inline int inlvarvariable2 = 2;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
namespace N
{
  int inline inlvarvariable3;				// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  const int inline inlvarvariable4 = 4;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
}
struct S
{
  static inline double inlvarvariable5 = 5.0;		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
#if __cplusplus >= 201103L
  static constexpr int inlvarvariable6 = 6;
  static inline constexpr int inlvarvariable7 = 7;	// { dg-warning "inline variables are only available with" "" { target { c++11 && c++14_down } } }
#endif
};
template <int N>					// { dg-warning "variable templates only available with" "" { target c++11_down } .+1 }
inline int inlvarvariable8;				// { dg-warning "inline variables are only available with" "" { target c++14_down } }
template <int N>					// { dg-warning "variable templates only available with" "" { target c++11_down } .+1 }
const int inline inlvarvariable9 = 9;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
namespace N
{
  template <int N>					// { dg-warning "variable templates only available with" "" { target c++11_down } .+1 }
  int inline inlvarvariable10 = 10;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  template <int N>					// { dg-warning "variable templates only available with" "" { target c++11_down } .+1 }
  const inline double inlvarvariable11 = 11.0;		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
}
template <int N>
struct T
{
  static inline int inlvarvariable12 = 12;		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
#if __cplusplus >= 201103L
  static constexpr int inlvarvariable13 = 13;
  static inline constexpr double inlvarvariable14 = 14.0; // { dg-warning "inline variables are only available with" "" { target { c++11 && c++14_down } } }
#endif
};
#if __cplusplus < 201103L
#define decltype(x) int
#endif
decltype (inlvarvariable1) v1 = inlvarvariable2 + sizeof (inlvarvariable1);
decltype (N::inlvarvariable3) v2 = N::inlvarvariable4 + sizeof (N::inlvarvariable3);
#if __cplusplus >= 201103L
decltype (S::inlvarvariable6) v3 = sizeof (S::inlvarvariable5) + S::inlvarvariable6 + S::inlvarvariable7;
#else
int v3 = sizeof (S::inlvarvariable5);
#endif
decltype (inlvarvariable8<2>) v4 = inlvarvariable9<2> + sizeof (inlvarvariable8<2>);
decltype (N::inlvarvariable10<0>) v5 = sizeof (N::inlvarvariable10<0>) + sizeof (N::inlvarvariable11<0>);
#if __cplusplus >= 201103L
decltype (T<-1>::inlvarvariable12) v6 = sizeof (T<-1>::inlvarvariable14) + sizeof (T<-1>::inlvarvariable12) + T<-1>::inlvarvariable13;
#else
int v6 = sizeof (T<-1>::inlvarvariable12);
#endif
