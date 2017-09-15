// { dg-do compile { target c++11 } }
// { dg-options "-Wdeprecated" }

inline int var1 = 4;				// { dg-warning "inline variables are only available with" "" { target c++14_down } }
static inline int var7 = 9;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
namespace N
{
  int inline var2;				// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  inline const int var6 = 8;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static inline double var8 = 2.0;		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  extern inline char var10;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
}
struct S
{
  static constexpr int var3 = 5;
  static inline int var4 = 6;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static constexpr int var5 = 7;
  static inline double var9 = 3.0;		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static constexpr inline int var11 = 11;	// { dg-warning "inline variables are only available with" "" { target c++14_down } }
};
const int S::var3;				// { dg-warning "redundant redeclaration of" "" { target c++17 } }
const int S::var3;				// { dg-error "redefinition of" "" { target c++14_down } }
extern int foo (int);				// { dg-warning "redundant redeclaration of" "" { target c++17 } .-1 }
extern int bar (int);
struct T { T () { t = foo (3); } T (int x) { t = foo (x); } int t; };
inline int var12 = foo (0);			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
int inline var13 = foo (1);			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
struct U
{
  static inline int var14 = foo (2);		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static inline T var15;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static inline T var16 = 4;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static int inline var17 = foo (5);		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static constexpr double var18 = 4.0;
};
extern inline int var19;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
extern inline int var20;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
int &ref19 = var19;				// { dg-error "odr-used inline variable 'var19' is not defined" "" { target *-*-* } .-2 }
int sz20 = sizeof (var20);
struct V
{
  static struct A var21;			// { dg-warning "inline variables are only available with" "" { target c++14_down } .+1 }
  static inline struct B var22;			// { dg-error "has incomplete type" }
  static inline struct C var23 = {};		// { dg-error "has incomplete type" }
};						// { dg-warning "inline variables are only available with" "" { target c++14_down } .-1 }
struct W
{
  static inline int var24;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static inline const int var25;		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
						// { dg-error "uninitialized const" "" { target *-*-* } .-1 }
  static inline int var26 = 5;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static inline const int var27 = 6;		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static inline double var28 = { 4.0 };		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static const inline double var29 = { 5.0 };	// { dg-warning "inline variables are only available with" "" { target c++14_down } }
};
int W::var24;					// { dg-error "redefinition of" }
const int W::var25;				// { dg-error "redefinition of" }
int W::var26;					// { dg-error "redefinition of" }
const int W::var27;				// { dg-error "redefinition of" }
double W::var28;				// { dg-error "redefinition of" }
double const W::var29;				// { dg-error "redefinition of" }
struct X
{
  inline int var30;				// { dg-error "'var30' declared as an 'inline' field" }
};
inline typedef int TT;				// { dg-error "'TT' declared as an 'inline' type" }
int
foo (inline int var31)				// { dg-error "'var31' declared as an 'inline' parameter" }
{
  inline int var32;				// { dg-error "'inline' specifier invalid for variable 'var32' declared at block scope" }
  static inline int var33;			// { dg-error "'inline' specifier invalid for variable 'var33' declared at block scope" }
}
template <typename A, typename B, typename C>
struct Y
{
  static A var34;				// { dg-warning "inline variables are only available with" "" { target c++14_down } .+1 }
  static inline B var35;			// { dg-error "has incomplete type" }
  static inline C var36;			// { dg-error "has incomplete type" }
};						// { dg-warning "inline variables are only available with" "" { target c++14_down } .-1 }
struct A;
struct B;
struct C;
Y<A, B, C> y;
A *ptr34 = &Y<A, B, C>::var34;
B *ptr35 = &Y<A, B, C>::var35;
C *ptr36 = &Y<A, B, C>::var36;
template <int N>
struct Z
{
  static inline int var37;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static inline const int var38;		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
						// { dg-error "uninitialized const" "" { target *-*-* } .-1 }
  static inline int var39 = 5;			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static inline const int var40 = 6;		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static inline double var41 = { 4.0 };		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static const inline double var42 = { 5.0 };	// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  static constexpr int var43 = 5;
  static constexpr inline int var44 = 5;	// { dg-warning "inline variables are only available with" "" { target c++14_down } }
};
template <int N>
int Z<N>::var37;				// { dg-error "redefinition of" }
template <int N>
const int Z<N>::var38;				// { dg-error "redefinition of" }
const int &ref38 = Z<0>::var38;
template <int N>
int Z<N>::var39;				// { dg-error "redefinition of" }
template <int N>
const int Z<N>::var40;				// { dg-error "redefinition of" }
template <int N>
double Z<N>::var41;				// { dg-error "redefinition of" }
template <int N>
double const Z<N>::var42;			// { dg-error "redefinition of" }
template <int N>
const int Z<N>::var43;				// { dg-warning "redundant redeclaration of" "" { target c++17 } }
template <int N>				// { dg-warning "redundant redeclaration of" "" { target c++17 } .+1 }
const int Z<N>::var43;				// { dg-error "redefinition of" "" { target c++14_down } }
Z<0> z;
