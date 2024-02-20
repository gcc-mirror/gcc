// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

using func_type_v0 = void(*)();

SA(   __is_invocable( func_type_v0 ) );
SA( ! __is_invocable( func_type_v0, int ) );

using func_type_i0 = int(*)();

SA(   __is_invocable( func_type_i0 ) );
SA( ! __is_invocable( func_type_i0, int ) );

using func_type_l0 = int&(*)();

SA(   __is_invocable( func_type_l0 ) );
SA( ! __is_invocable( func_type_l0(int) ) );

using func_type_ii = int(*)(int);

SA( ! __is_invocable( func_type_ii ) );
SA(   __is_invocable( func_type_ii, int ) );

using func_type_il = int(*)(int&);

SA( ! __is_invocable( func_type_il ) );
SA( ! __is_invocable( func_type_il, int ) );
SA(   __is_invocable( func_type_il, int& ) );

using func_type_ir = int(*)(int&&);

SA( ! __is_invocable( func_type_ir ) );
SA( ! __is_invocable( func_type_ir, int& ) );
SA(   __is_invocable( func_type_ir, int ) );
SA(   __is_invocable( func_type_ir, int&& ) );

struct A { };

using mem_type_i = int A::*;

SA( ! __is_invocable( mem_type_i ) );
SA( ! __is_invocable( mem_type_i, int ) );
SA( ! __is_invocable( mem_type_i, int* ) );
SA( ! __is_invocable( mem_type_i, int& ) );
SA( ! __is_invocable( mem_type_i, int&& ) );
SA(   __is_invocable( mem_type_i, A ) );
SA(   __is_invocable( mem_type_i, A* ) );
SA(   __is_invocable( mem_type_i, A& ) );
SA(   __is_invocable( mem_type_i, A&& ) );
SA(   __is_invocable( mem_type_i, const A& ) );
SA( ! __is_invocable( mem_type_i, A&, int ) );

using memfun_type_i = int (A::*)();

SA( ! __is_invocable( memfun_type_i ) );
SA( ! __is_invocable( memfun_type_i, int ) );
SA( ! __is_invocable( memfun_type_i, int* ) );
SA( ! __is_invocable( memfun_type_i, int& ) );
SA( ! __is_invocable( memfun_type_i, int&& ) );
SA(   __is_invocable( memfun_type_i, A ) );
SA(   __is_invocable( memfun_type_i, A* ) );
SA(   __is_invocable( memfun_type_i, A& ) );
SA(   __is_invocable( memfun_type_i, A&& ) );
SA( ! __is_invocable( memfun_type_i, const A& ) );
SA( ! __is_invocable( memfun_type_i, A&, int ) );

using memfun_type_ic = int (A::*)() const;

SA( ! __is_invocable( memfun_type_ic ) );
SA( ! __is_invocable( memfun_type_ic, int ) );
SA( ! __is_invocable( memfun_type_ic, int& ) );
SA(   __is_invocable( memfun_type_ic, A& ) );
SA(   __is_invocable( memfun_type_ic, A* ) );
SA( ! __is_invocable( memfun_type_ic, A&, int ) );
SA( ! __is_invocable( memfun_type_ic, A*, int& ) );
SA(   __is_invocable( memfun_type_ic, const A& ) );
SA(   __is_invocable( memfun_type_ic, const A* ) );
SA( ! __is_invocable( memfun_type_ic, const A&, int& ) );
SA( ! __is_invocable( memfun_type_ic, const A*, int ) );

using memfun_type_iic = int& (A::*)(int&) const;

SA( ! __is_invocable( memfun_type_iic ) );
SA( ! __is_invocable( memfun_type_iic, int ) );
SA( ! __is_invocable( memfun_type_iic, int& ) );
SA( ! __is_invocable( memfun_type_iic, A&, int ) );
SA(   __is_invocable( memfun_type_iic, A&, int& ) );
SA( ! __is_invocable( memfun_type_iic, A*, int ) );
SA(   __is_invocable( memfun_type_iic, A*, int& ) );
SA( ! __is_invocable( memfun_type_iic, const A&, int ) );
SA( ! __is_invocable( memfun_type_iic, const A&, int&, int ) );
SA(   __is_invocable( memfun_type_iic, const A&, int& ) );
SA(   __is_invocable( memfun_type_iic, const A*, int& ) );

struct B {
  int& operator()();
  long& operator()() const;
  bool& operator()(int);
private:
  void operator()(int, int);
};
using CB = const B;

SA(   __is_invocable( B ) );
SA(   __is_invocable( B& ) );
SA(   __is_invocable( B&& ) );
SA( ! __is_invocable( B* ) );
SA(   __is_invocable( CB ) );
SA(   __is_invocable( CB& ) );
SA( ! __is_invocable( CB* ) );

SA(   __is_invocable( B, int ) );
SA(   __is_invocable( B&, int ) );
SA(   __is_invocable( B&&, int ) );
SA( ! __is_invocable( B*, int ) );
SA( ! __is_invocable( CB, int ) );
SA( ! __is_invocable( CB&, int ) );
SA( ! __is_invocable( CB*, int ) );

SA( ! __is_invocable( B, int, int ) );
SA( ! __is_invocable( B&, int, int ) );
SA( ! __is_invocable( B&&, int, int ) );
SA( ! __is_invocable( B*, int, int ) );
SA( ! __is_invocable( CB, int, int ) );
SA( ! __is_invocable( CB&, int, int ) );
SA( ! __is_invocable( CB*, int, int ) );

struct C : B { int& operator()() = delete; };
using CC = const C;

SA( ! __is_invocable( C ) );
SA( ! __is_invocable( C& ) );
SA( ! __is_invocable( C&& ) );
SA( ! __is_invocable( C* ) );
SA( ! __is_invocable( CC ) );
SA( ! __is_invocable( CC& ) );
SA( ! __is_invocable( CC* ) );

struct D { B operator*(); };
using CD = const D;

SA( ! __is_invocable( D ) );

struct E { void v(); };
using CE = const E;

SA( ! __is_invocable( E ) );
SA( ! __is_invocable( void (E::*)() ) );
SA(   __is_invocable( void (E::*)(), E ) );
SA(   __is_invocable( void (E::*)(), E* ) );
SA( ! __is_invocable( void (E::*)(), CE ) );

struct F : E {};
using CF = const F;

SA( ! __is_invocable( F ) );
SA(   __is_invocable( void (E::*)(), F ) );
SA(   __is_invocable( void (E::*)(), F* ) );
SA( ! __is_invocable( void (E::*)(), CF ) );

struct G { E operator*(); };
using CG = const G;

SA( ! __is_invocable( G ) );
SA(   __is_invocable( void (E::*)(), G ) );
SA( ! __is_invocable( void (E::*)(), G* ) );
SA( ! __is_invocable( void (E::*)(), CG ) );

struct H { E& operator*(); };
using CH = const H;

SA( ! __is_invocable( H ) );
SA(   __is_invocable( void (E::*)(), H ) );
SA( ! __is_invocable( void (E::*)(), H* ) );
SA( ! __is_invocable( void (E::*)(), CH ) );

struct I { E&& operator*(); };
using CI = const I;

SA( ! __is_invocable( I ) );
SA(   __is_invocable( void (E::*)(), I ) );
SA( ! __is_invocable( void (E::*)(), I* ) );
SA( ! __is_invocable( void (E::*)(), CI ) );

struct K { E* operator*(); };
using CK = const K;

SA( ! __is_invocable( K ) );
SA( ! __is_invocable( void (E::*)(), K ) );
SA( ! __is_invocable( void (E::*)(), K* ) );
SA( ! __is_invocable( void (E::*)(), CK ) );

struct L { CE operator*(); };
using CL = const L;

SA( ! __is_invocable( L ) );
SA( ! __is_invocable( void (E::*)(), L ) );
SA( ! __is_invocable( void (E::*)(), L* ) );
SA( ! __is_invocable( void (E::*)(), CL ) );

struct M {
  int i;
private:
  long l;
};
using CM = const M;

SA( ! __is_invocable( M ) );
SA( ! __is_invocable( M& ) );
SA( ! __is_invocable( M&& ) );
SA( ! __is_invocable( M* ) );
SA( ! __is_invocable( CM ) );
SA( ! __is_invocable( CM& ) );
SA( ! __is_invocable( CM* ) );

SA( ! __is_invocable( int M::* ) );
SA(   __is_invocable( int M::*, M ) );
SA(   __is_invocable( int M::*, M& ) );
SA(   __is_invocable( int M::*, M&& ) );
SA(   __is_invocable( int M::*, M* ) );
SA(   __is_invocable( int M::*, CM ) );
SA(   __is_invocable( int M::*, CM& ) );
SA(   __is_invocable( int M::*, CM* ) );
SA( ! __is_invocable( int M::*, int ) );

SA( ! __is_invocable( int CM::* ) );
SA(   __is_invocable( int CM::*, M ) );
SA(   __is_invocable( int CM::*, M& ) );
SA(   __is_invocable( int CM::*, M&& ) );
SA(   __is_invocable( int CM::*, M* ) );
SA(   __is_invocable( int CM::*, CM ) );
SA(   __is_invocable( int CM::*, CM& ) );
SA(   __is_invocable( int CM::*, CM* ) );
SA( ! __is_invocable( int CM::*, int ) );

SA( ! __is_invocable( long M::* ) );
SA(   __is_invocable( long M::*, M ) );
SA(   __is_invocable( long M::*, M& ) );
SA(   __is_invocable( long M::*, M&& ) );
SA(   __is_invocable( long M::*, M* ) );
SA(   __is_invocable( long M::*, CM ) );
SA(   __is_invocable( long M::*, CM& ) );
SA(   __is_invocable( long M::*, CM* ) );
SA( ! __is_invocable( long M::*, long ) );

SA( ! __is_invocable( long CM::* ) );
SA(   __is_invocable( long CM::*, M ) );
SA(   __is_invocable( long CM::*, M& ) );
SA(   __is_invocable( long CM::*, M&& ) );
SA(   __is_invocable( long CM::*, M* ) );
SA(   __is_invocable( long CM::*, CM ) );
SA(   __is_invocable( long CM::*, CM& ) );
SA(   __is_invocable( long CM::*, CM* ) );
SA( ! __is_invocable( long CM::*, long ) );

SA( ! __is_invocable( short M::* ) );
SA(   __is_invocable( short M::*, M ) );
SA(   __is_invocable( short M::*, M& ) );
SA(   __is_invocable( short M::*, M&& ) );
SA(   __is_invocable( short M::*, M* ) );
SA(   __is_invocable( short M::*, CM ) );
SA(   __is_invocable( short M::*, CM& ) );
SA(   __is_invocable( short M::*, CM* ) );
SA( ! __is_invocable( short M::*, short ) );

SA( ! __is_invocable( short CM::* ) );
SA(   __is_invocable( short CM::*, M ) );
SA(   __is_invocable( short CM::*, M& ) );
SA(   __is_invocable( short CM::*, M&& ) );
SA(   __is_invocable( short CM::*, M* ) );
SA(   __is_invocable( short CM::*, CM ) );
SA(   __is_invocable( short CM::*, CM& ) );
SA(   __is_invocable( short CM::*, CM* ) );
SA( ! __is_invocable( short CM::*, short ) );

struct N { M operator*(); };
SA(   __is_invocable( int M::*, N ) );
SA( ! __is_invocable( int M::*, N* ) );

struct O { M& operator*(); };
SA(   __is_invocable( int M::*, O ) );
SA( ! __is_invocable( int M::*, O* ) );

struct P { M&& operator*(); };
SA(   __is_invocable( int M::*, P ) );
SA( ! __is_invocable( int M::*, P* ) );

struct Q { M* operator*(); };
SA( ! __is_invocable( int M::*, Q ) );
SA( ! __is_invocable( int M::*, Q* ) );

struct R { void operator()(int = 0); };

SA(   __is_invocable( R ) );
SA(   __is_invocable( R, int ) );
SA( ! __is_invocable( R, int, int ) );

struct S { void operator()(int, ...); };

SA( ! __is_invocable( S ) );
SA(   __is_invocable( S, int ) );
SA(   __is_invocable( S, int, int ) );
SA(   __is_invocable( S, int, int, int ) );

void fn1() {}

SA(   __is_invocable( decltype(fn1) ) );

void fn2(int arr[10]);

SA(   __is_invocable( decltype(fn2), int[10] ) );
SA(   __is_invocable( decltype(fn2), int(&)[10] ) );
SA(   __is_invocable( decltype(fn2), int(&&)[10] ) );
SA( ! __is_invocable( decltype(fn2), int(*)[10] ) );
SA( ! __is_invocable( decltype(fn2), int(*&)[10] ) );
SA( ! __is_invocable( decltype(fn2), int(*&&)[10] ) );
SA(   __is_invocable( decltype(fn2), int[] ) );

auto lambda = []() {};

SA(   __is_invocable( decltype(lambda) ) );

template <typename Func, typename... Args>
struct can_invoke {
    static constexpr bool value = __is_invocable( Func, Args... );
};

SA(   can_invoke<decltype(lambda)>::value );

struct T {
  void func() const {}
  int data;
};

SA(   __is_invocable( decltype(&T::func)&, T& ) );
SA(   __is_invocable( decltype(&T::data)&, T& ) );

struct U { };
struct V : U { U& operator*() = delete; };
SA(   __is_invocable( int U::*, V ) );

struct W : private U { U& operator*(); };
SA( ! __is_invocable( int U::*, W ) );

struct X { int m; };
struct Y { X& operator*(); };
struct Z : Y { };
SA(   __is_invocable(int X::*, Z) );
