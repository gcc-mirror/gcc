/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

typedef long size_t;
typedef signed char int8_t;
typedef  char uint8_t

;
template < typename > struct Relations{ using Unsigned = uint8_t; };
template < typename T > using MakeUnsigned = typename Relations< T >::Unsigned;
#pragma riscv intrinsic "vector"
size_t ScaleByPower() {  return 0;}
template < typename Lane, size_t , int > struct Simd {
using T = Lane;

template < typename NewT > using Rebind = Simd< NewT, 1, 0 >;
};
template < typename T > struct ClampNAndPow2 {
using type = Simd< T, 65536, 0 >
;
};
struct CappedTagChecker {
using type = ClampNAndPow2< signed char >::type;
};
template < typename , size_t , int >
using CappedTag = CappedTagChecker::type;
template < class D > using TFromD = typename D::T;
template < class T, class D > using Rebind = typename D::Rebind< T >;
template < class D >
using RebindToUnsigned = Rebind< MakeUnsigned<  D  >, D >;
template < size_t N >
size_t
Lanes(Simd< uint8_t, N, 0 > ) {
size_t kFull = 0;
size_t kCap ;
size_t actual =
        __riscv_vsetvl_e8m1(kCap);
return actual;
}
template < size_t N >
size_t
Lanes(Simd< int8_t, N, 0 > ) {
size_t kFull  ;
size_t kCap ;
size_t actual =
        __riscv_vsetvl_e8m1(kCap);
return actual;
}
template < size_t N >
vuint8m1_t
Set(Simd< uint8_t, N, 0 > d, uint8_t arg) {
size_t __trans_tmp_1 = Lanes(d);
return __riscv_vmv_v_x_u8m1(arg, __trans_tmp_1);
}
template < size_t N >
vint8m1_t Set(Simd< int8_t, N, 0 > , int8_t );
template < class D > using VFromD = decltype(Set(D(), TFromD< D >()));
template < class D >
VFromD< D >
Zero(D )
;

template < size_t N >
vint8m1_t
BitCastFromByte(Simd< int8_t, N, 0 >, vuint8m1_t v) {
return __riscv_vreinterpret_v_u8m1_i8m1(v);
}
template < class D, class FromV >
VFromD< D >
BitCast(D d, FromV v) {
return BitCastFromByte(d, v)

;
}
template < size_t N >
void
Store(vint8m1_t v, Simd< int8_t, N, 0 > d) {
int8_t *p ;
__riscv_vse8_v_i8m1(p, v, Lanes(d));
}
template < class V, class D >
void
StoreU(V v, D d) {
Store(v, d)
;
}
template < class D > using Vec = decltype(Zero(D()));
size_t Generate_count;
template < class D, class Func>
void Generate(D d, Func func) {
RebindToUnsigned< D > du
;
size_t N = Lanes(d);
Vec< decltype(du) > vidx ;
for (; ; ) {
   StoreU(func(d, vidx), d);
   vidx = (Set(du, N));
}
}
template < typename T, int , int kMinArg, class Test, int kPow2 >
struct ForeachCappedR {
static void Do(size_t , size_t ) {
   CappedTag< T, kMinArg, kPow2 > d;
   Test()(T(), d);
}
};
template < class > struct ForeachCountAndMisalign;
struct TestGenerate;
template < int kPow2 = 1 > class ForExtendableVectors {
public:

template < typename T > void operator()(T) {
   size_t max_lanes  ;
   ForeachCappedR< T, 0, size_t{} ,
                   ForeachCountAndMisalign< int >, kPow2 >::Do(1, max_lanes);
}
};
class ForPartialVectors {
public:
template < typename T > void operator()(T t)  {
   ForExtendableVectors()(t);
}
};
void ForSignedTypes(ForPartialVectors func) { func(int8_t()); }
template < class > struct ForeachCountAndMisalign {
template < typename T, class D >
void operator()(T, D d) {
   int rng
   ;
   size_t misalignments[1] ;
   for (   size_t ma : misalignments)
       for (size_t mb : misalignments)
         TestGenerate()(d, 0, ma, mb, rng);
}
};
struct TestGenerate {
template < class D >
void operator()(D d, size_t , size_t , size_t, int ) {
   auto gen2 = [](auto d, auto vidx) {
     return BitCast(d, (vidx));
   };
   Generate(d, gen2);
}
};
