/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O2" } */

#include<cmath>
#include<cstddef>
#include<cstdint>
#include<iomanip>
#include<ios>
#include<iostream>
#include<memory>
#include<type_traits>
#include"riscv_vector.h"
using std::addressof;
using std::cerr;
using std::endl;
using std::int8_t;
using std::int16_t;
using std::int32_t;
using std::int64_t;
using std::uint8_t;
using std::uint16_t;
using std::uint32_t;
using std::uint64_t;
using std::ptrdiff_t;
using std::size_t;
using float16_t = _Float16;
using float32_t = float;
using float64_t = double;

template<class T, class T2>
constexpr T uint_to_float(T2 val) noexcept
{
  return *reinterpret_cast<T*>(&val);
}

constexpr const auto &f16(uint_to_float<float16_t, uint16_t>);
constexpr const auto &f32(uint_to_float<float32_t, uint32_t>);
constexpr const auto &f64(uint_to_float<float64_t, uint64_t>);

template<class T>
struct To_uint
{
  using type = std::conditional_t<
    sizeof(T) == 1, uint8_t, std::conditional_t<
      sizeof(T) == 2, uint16_t, std::conditional_t<
        sizeof(T) == 4, uint32_t, std::conditional_t<
          sizeof(T) == 8, uint64_t, void
        >
      >
    >
  >;
};

// isnan() does not support half type
template<class T>
struct To_float
{
  using type = std::conditional_t<
    std::is_same<T, float16_t>::value, float, std::conditional_t<
      std::is_same<T, float32_t>::value, float, std::conditional_t<
        std::is_same<T, float64_t>::value, double, float
      >
    >
  >;
};

template<class T>
using To_uint_t = typename To_uint<T>::type;

template<class T>
using To_isnan_float = typename To_float<T>::type;

template <class T>
void print_float(std::ostream &os, T val)
{
  using std::setw;
  os << std::hex << std::setfill('0') << setw(sizeof(T) * 2) << *reinterpret_cast<To_uint_t<T>*>(addressof(val)) << setw(0) << std::dec;
}

template <class T>
bool __attribute__((noinline))
check(const T *a, const T *b, size_t size)
{
  bool rv = true;
  for (size_t i = 0; i < (size / sizeof(T)); ++i) {
    if (reinterpret_cast<const To_uint_t<T> *>(a)[i] ==
        reinterpret_cast<const To_uint_t<T> *>(b)[i])
      continue;
    // floating negative zero == positive zero
    if ((std::is_floating_point_v<T> || std::is_same<T, float16_t>::value) &&
        (a[i] == b[i]))
      continue;
    // if both result are NaN, return true
    if ((std::is_same<T, float16_t>::value || std::is_floating_point_v<T>) &&
         std::isnan(static_cast<To_isnan_float<T>>(a[i])) &&
         std::isnan(static_cast<To_isnan_float<T>>(b[i])))
      continue;

    if (std::is_same<T, float16_t>::value) {
      cerr << std::hex << std::setfill('0') << std::setw(sizeof(T) * 2) << "["
           << i
           << "] result/golden:" << reinterpret_cast<const To_uint_t<T> *>(a)[i]
           << " != " << reinterpret_cast<const To_uint_t<T> *>(b)[i]
           << std::setw(0) << std::dec << endl;
    } else if constexpr (std::is_floating_point_v<T>) {
      cerr << "[" << i << "] result/golden:" << a[i] << "(";
      print_float(cerr, a[i]);
      cerr << ") != " << b[i] << "(";
      print_float(cerr, b[i]);
      cerr << ")" << endl;
    } else if constexpr (std::is_unsigned_v<T>) {
      cerr << "[" << i << "] result/golden: " << static_cast<uintmax_t>(a[i])
           << " != " << static_cast<uintmax_t>(b[i]) << endl;
    } else {
      cerr << "[" << i << "] result/golden:" << static_cast<intmax_t>(a[i])
           << " != " << static_cast<intmax_t>(b[i]) << endl;
    }
    rv = false;
  }
  return rv;
}

template <class T>
bool __attribute__((noinline))
check(const T a, const T golden)
{
  return check(addressof(a), addressof(golden), sizeof(T));
}

int main()
{
int return_value = 0;

size_t var_108 = 32u;
uint32_t var_107 [] = {2032328274u, 626526942u, 441945600u, 359218908u, 795467553u, 481030164u, 1519533270u, 876226206u};
size_t var_105 = 14u;
int16_t var_104 [] = {26664, -5751, -27825, 26664, 13617, 13617, -6442};
size_t var_102 = 16u;
int16_t var_101 [] = {-9364, 32767, 11538, -10536, 32767, 30906, 30906, 4977};
size_t var_99 = 4u;
int32_t var_98 [] = {-967692376};
uint32_t var_96 [] = {200442261u, 4280292277u, 1765883272u, 3011489556u, 3298976744u, 2700996695u, 1391342474u, 1658482804u};
int16_t var_95 [] = {-25088, 32020, -14555, 29922, -27825, 16446, 10946, 26664, -26653, -9607, -12312, -5751, 7191, -9395, 4649, 13617};
uint32_t var_93 [] = {16u, 14u, 22u, 8u, 14u, 30u, 30u, 20u};
int32_t var_91 [] = {-1676424092};
uint16_t var_90 [] = {32717u, 49606u, 44536u, 45400u, 20945u, 52615u, 58914u};
int32_t var_89 [] = {1150382723, -25813573, -1544838363, 11637069, -1756987316, 581238726, -642314632};
int32_t var_88 [] = {1152185805, 966606079, -1650031826, -1900183008, 31785867, 2085283094, 486945939};
uint32_t var_87 [] = {603695150u, 709791116u, 293373582u, 2465486296u, 3629298997u, 3700014134u, 3947999496u};
int16_t var_86 [] = {27299, 9863, -25877, -3835, -12966, 5361, -614};
int16_t var_85 [] = {9130, -28060, -31697, 2392, -18280, 5054, -10904};
int16_t var_84 [] = {-30726, 3935, -32544, 30984, -7828, -13821, -24051};
uint16_t var_83 [] = {34622u, 47429u, 38572u, 54107u, 37111u, 7090u, 15572u};
uint16_t var_82 [] = {23590u, 22627u, 63394u, 26694u, 46543u, 45392u, 47987u};
int8_t var_81 [] = {-46, -43, -66, 102, 75, -106, -78};
int16_t var_80 [] = {-6504, 17213, -7465, -12809, -22594, -5977, 28325};
int16_t var_79 [] = {-25308, -29748, 964, 5301, -14548, -8172, 16870, -12725};
uint32_t var_78 [] = {2749387368u, 34851750u, 1821162092u, 3392410812u, 225949308u, 3649011883u, 1520252640u, 205467072u};
float32_t var_77 [] = {f32(371865542u), f32(1636635715u), f32(4109999702u), f32(1019721516u), f32(4263035649u), f32(2332917959u), f32(4059235545u), f32(847467537u)};
// 1.3750089631844789e-25, 3.253063656371556e+20, -1.5821452052179264e+32, 0.024379335343837738, -5.076379338228278e+37, -2.725249352151892e-32, -2.406742970988547e+30, 1.5288636134869193e-08

int32_t var_76 [] = {948862036, -2093729137, 963404136, 1242496314, 1677591169, 1057599172, 985156, 1851640545};
uint16_t var_75 [] = {8495u, 45225u, 33771u, 3769u, 4768u, 28948u, 504u};
uint8_t var_74 [] = {153u, 78u, 249u, 147u, 33u, 209u, 121u};
int16_t var_73 [] = {-6442, 2757, 1437, -18340, -5849, -27551, 29648};
int16_t var_72 [] = {29981, -14262, 26356, 1811, -10931, 24794, -29367};
uint16_t var_71 [] = {58862u, 18146u, 12800u, 10404u, 23039u, 13932u, 44010u, 25378u};
int8_t var_70 [] = {-25, 3, 102, 71, 11, 11, -11};
int16_t var_69 [] = {12911, 12016, -31202, -226, 12729, 6375, 30653};
int32_t var_68 [] = {2060611965, 1606981125, -198439603, -1488521294, 1133517116, -2044388464, 1526469553, -1251736891};
int32_t var_67 [] = {-868848602, 735716445, -1200143269, -1475028242, -1315587111, -822562929, -1719354165, 575969997};
int16_t var_66 [] = {-32336, 24855, -22214, 255, -19505, -4998, -2438};
size_t var_65 = 4u;
uint16_t var_64 = 34527u;
int16_t var_63 = -14406;
int16_t var_62 = -6468;
uint16_t var_61 = 18798u;
int16_t var_60 = 17289;
size_t var_59 = 6u;
int32_t var_58 = 182690347;
int16_t var_57 = -8819;
int32_t var_56 = -720640033;
uint8_t var_55 = 68u;
uint16_t var_54 = 44422u;
int8_t var_53 = -20;
__riscv_vsetvl_e32m2(8);
vuint32m2_t var_92 = __riscv_vle32_v_u32m2(var_93, 8);
// 16, 14, 22, 8, 14, 30, 30, 20

__riscv_vsetvl_e16m8(7);
vuint16m8_t var_21 = __riscv_vle16_v_u16m8(var_90, 7);
// 32717, 49606, 44536, 45400, 20945, 52615, 58914

__riscv_vsetvl_e32m2(7);
vint32m2_t var_22 = __riscv_vle32_v_i32m2(var_89, 7);
// 1150382723, -25813573, -1544838363, 11637069, -1756987316, 581238726, -642314632

vint32m2_t var_23 = __riscv_vle32_v_i32m2(var_88, 7);
// 1152185805, 966606079, -1650031826, -1900183008, 31785867, 2085283094, 486945939

vuint32m2_t var_24 = __riscv_vle32_v_u32m2(var_87, 7);
// 603695150, 709791116, 293373582, 2465486296, 3629298997, 3700014134, 3947999496

vint16m1_t var_25 = __riscv_vle16_v_i16m1(var_86, 7);
// 27299, 9863, -25877, -3835, -12966, 5361, -614

vint16m1_t var_26 = __riscv_vle16_v_i16m1(var_85, 7);
// 9130, -28060, -31697, 2392, -18280, 5054, -10904

__riscv_vsetvl_e16m8(7);
vint16m8_t var_29 = __riscv_vle16_v_i16m8(var_84, 7);
// -30726, 3935, -32544, 30984, -7828, -13821, -24051

vuint16m8_t var_30 = __riscv_vle16_v_u16m8(var_83, 7);
// 34622, 47429, 38572, 54107, 37111, 7090, 15572

vuint16m8_t var_31 = __riscv_vle16_v_u16m8(var_82, 7);
// 23590, 22627, 63394, 26694, 46543, 45392, 47987

vint8m4_t var_32 = __riscv_vle8_v_i8m4(var_81, 7);
// -46, -43, -66, 102, 75, -106, -78

__riscv_vsetvl_e16m1(7);
vint16m1_t var_33 = __riscv_vle16_v_i16m1(var_80, 7);
// -6504, 17213, -7465, -12809, -22594, -5977, 28325

__riscv_vsetvl_e32m2(8);
vuint32m2_t var_36 = __riscv_vle32_v_u32m2(var_78, 8);
// 2749387368, 34851750, 1821162092, 3392410812, 225949308, 3649011883, 1520252640, 205467072

vfloat32m2_t var_37 = __riscv_vle32_v_f32m2(var_77, 8);
// 1.3750089631844789e-25, 3.253063656371556e+20, -1.5821452052179264e+32, 0.024379335343837738, -5.076379338228278e+37, -2.725249352151892e-32, -2.406742970988547e+30, 1.5288636134869193e-08

vint32m2_t var_38 = __riscv_vle32_v_i32m2(var_76, 8);
// 948862036, -2093729137, 963404136, 1242496314, 1677591169, 1057599172, 985156, 1851640545

__riscv_vsetvl_e16m1(7);
vuint16m1_t var_41 = __riscv_vle16_v_u16m1(var_75, 7);
// 8495, 45225, 33771, 3769, 4768, 28948, 504

vuint8mf2_t var_42 = __riscv_vle8_v_u8mf2(var_74, 7);
// 153, 78, 249, 147, 33, 209, 121

__riscv_vsetvl_e16m8(7);
vint16m8_t var_43 = __riscv_vle16_v_i16m8(var_73, 7);
// -6442, 2757, 1437, -18340, -5849, -27551, 29648

vint16m8_t var_44 = __riscv_vle16_v_i16m8(var_72, 7);
// 29981, -14262, 26356, 1811, -10931, 24794, -29367

__riscv_vsetvl_e16m1(8);
vuint16m1_t var_45 = __riscv_vle16_v_u16m1(var_71, 8);
// 58862, 18146, 12800, 10404, 23039, 13932, 44010, 25378

__riscv_vsetvl_e8mf2(7);
vint8mf2_t var_47 = __riscv_vle8_v_i8mf2(var_70, 7);
// -25, 3, 102, 71, 11, 11, -11

__riscv_vsetvl_e16m8(7);
vint16m8_t var_48 = __riscv_vle16_v_i16m8(var_69, 7);
// 12911, 12016, -31202, -226, 12729, 6375, 30653

__riscv_vsetvl_e32m2(8);
vint32m2_t var_50 = __riscv_vle32_v_i32m2(var_68, 8);
// 2060611965, 1606981125, -198439603, -1488521294, 1133517116, -2044388464, 1526469553, -1251736891

vint32m2_t var_51 = __riscv_vle32_v_i32m2(var_67, 8);
// -868848602, 735716445, -1200143269, -1475028242, -1315587111, -822562929, -1719354165, 575969997

vint16m1_t var_0 = __riscv_vloxei32_v_i16m1(var_95, var_92, 8);
// -26653, 26664, -5751, -27825, 26664, 13617, 13617, -12312

__riscv_vsetvl_e16m8(7);
vuint16m8_t var_18 = __riscv_vrgather_vx_u16m8(var_21, var_59, 7);
// 58914, 58914, 58914, 58914, 58914, 58914, 58914

__riscv_vsetvl_e16m1(7);
vint16m1_t var_16 = __riscv_vor_vv_i16m1(var_25, var_26, 7);
// 27563, -18713, -24849, -1699, -550, 6143, -518

__riscv_vsetvl_e16m8(7);
vbool2_t var_28 = __riscv_vmsbc_vx_i16m8_b2(var_29, var_57, 7);
// 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0

vbool2_t var_27 = __riscv_vmsgeu_vv_u16m8_b2(var_30, var_31, 7);
// 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32m2(8);
vbool16_t var_35 = __riscv_vmsbc_vx_i32m2_b16(var_38, var_56, 8);
// 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e16m1(7);
vbool16_t var_40 = __riscv_vmsgtu_vx_u16m1_b16(var_41, var_54, 7);
// 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vbool16_t var_39 = __riscv_vmadc_vx_u8mf2_b16(var_42, var_55, 7);
// 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e16m1(8);
vuint32m2_t var_7 = __riscv_vwmulu_vx_u32m2(var_45, var_64, 8);
// 2032328274, 626526942, 441945600, 359218908, 795467553, 481030164, 1519533270, 876226206

__riscv_vsetvl_e8mf2(7);
vbool16_t var_46 = __riscv_vmsle_vx_i8mf2_b16(var_47, var_53, 7);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32m2(8);
vbool16_t var_49 = __riscv_vmseq_vv_i32m2_b16(var_50, var_51, 8);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vint16m1_t var_13 = __riscv_vsadd_vx_i16m1(var_0, var_60, 8);
// -9364, 32767, 11538, -10536, 32767, 30906, 30906, 4977

__riscv_vsetvl_e16m8(7);
vbool2_t var_11 = __riscv_vmsgeu_vx_u16m8_b2(var_18, var_61, 7);
// 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0

vint16m1_t var_14 = __riscv_vwredsum_vs_i8m4_i16m1_tu(var_16, var_32, var_33, 7);
// -6666

vbool2_t var_15 = __riscv_vmnor_mm_b2(var_27, var_28, 7);
// 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32m2(8);
vuint32m2_t var_12 = __riscv_vfcvt_rtz_xu_f_v_u32m2_mu(var_35, var_36, var_37, 8);
// 0, 4294967295, 0, 0, 0, 0, 0, 0

__riscv_vse16_v_i16m1(var_79, var_13, 8);
__riscv_vsetvl_e16m8(7);
vint16m8_t var_9 = __riscv_vaadd_vx_i16m8_mu(var_15, var_43, var_44, var_63, 0, 7);
// -6442, 2757, 1437, -18340, -12668, -27551, 29648

__riscv_vsetvl_e32m2(8);
vuint32m2_t var_3 = __riscv_vsrl_vx_u32m2_mu(var_49, var_7, var_12, var_65, 8);
// 2032328274, 626526942, 441945600, 359218908, 795467553, 481030164, 1519533270, 876226206

if(!check(var_79, var_101, var_102)) {cerr << "check 100 fails" << endl; return_value = 1;}
__riscv_vsetvl_e16m8(7);
int16_t var_8 = __riscv_vmv_x_s_i16m8_i16(var_9);
// -6442

__riscv_vsetvl_e32m2(8);
__riscv_vse32_v_u32m2(var_96, var_3, 8);
__riscv_vsetvl_e16m8(7);
vint16m8_t var_4 = __riscv_vmerge_vxm_i16m8(var_48, var_8, var_11, 7);
// -6442, -6442, -6442, -6442, -6442, -6442, -6442

__riscv_vsetvl_e16m1(1);
vint16m1_t var_6 = __riscv_vaadd_vx_i16m1(var_14, var_8, 0, 1);
// -6554

if(!check(var_96, var_107, var_108)) {cerr << "check 106 fails" << endl; return_value = 1;}
__riscv_vsetvl_e16m8(7);
int16_t var_2 = __riscv_vmv_x_s_i16m8_i16(var_4);
// -6442

__riscv_vsetvl_e16m1(1);
vint16m1_t var_5 = __riscv_vredsum_vs_i16m1_i16m1_tum(var_46, var_0, var_6, var_6, 1);
// -13108

__riscv_vsetvl_e16m1(7);
vint16m1_t var_1 = __riscv_vslide1down_vx_i16m1(var_0, var_2, 7);
// 26664, -5751, -27825, 26664, 13617, 13617, -6442

__riscv_vsetvl_e16m1(1);
vbool16_t var_10 = __riscv_vmsgt_vx_i16m1_b16_mu(var_39, var_40, var_5, var_62, 1);
// 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e16m1(7);
__riscv_vse16_v_i16m1(var_66, var_1, 7);
__riscv_vsetvl_e32m2(1);
vint32m2_t var_17 = __riscv_vsra_vv_i32m2_mu(var_10, var_22, var_23, var_24, 1);
// 1150382723, -25813573, -1544838363, 11637069, -1756987316, 581238726, -642314632

if(!check(var_66, var_104, var_105)) {cerr << "check 103 fails" << endl; return_value = 1;}
vint32m2_t var_19 = __riscv_vrsub_vx_i32m2(var_17, var_58, 1);
// -967692376

__riscv_vse32_v_i32m2(var_91, var_19, 1);
if(!check(var_91, var_98, var_99)) {cerr << "check 97 fails" << endl; return_value = 1;}
if (return_value)
  __builtin_abort ();
return return_value;
}
