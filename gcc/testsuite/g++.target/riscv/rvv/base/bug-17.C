/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O2" } */

#include<cstdalign>
#include<cmath>
#include<cstddef>
#include<cstdint>
#include<iomanip>
#include<ios>
#include<iostream>
#include<memory>
#include<type_traits>
#include<riscv_vector.h>

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

#if defined(__riscv_xsfvfhbfmin) || defined(__riscv_xsfvfwmaccqqq)
#define vle16_v_bf16mf4(p, vl) vreinterpret_v_u16mf4_bf16mf4(vle16_v_u16mf4(p, vl))
#define vle16_v_bf16mf2(p, vl) vreinterpret_v_u16mf2_bf16mf2(vle16_v_u16mf2(p, vl))
#define vle16_v_bf16m1(p, vl) vreinterpret_v_u16m1_bf16m1(vle16_v_u16m1(p, vl))
#define vle16_v_bf16m2(p, vl) vreinterpret_v_u16m2_bf16m2(vle16_v_u16m2(p, vl))
#define vle16_v_bf16m4(p, vl) vreinterpret_v_u16m4_bf16m4(vle16_v_u16m4(p, vl))
#define vle16_v_bf16m8(p, vl) vreinterpret_v_u16m8_bf16m8(vle16_v_u16m8(p, vl))
#define __riscv_vse16_v_bf16mf4(p, v, vl) __riscv_vse16_v_u16mf4(p, vreinterpret_v_bf16mf4_u16mf4(v), vl)
#define __riscv_vse16_v_bf16mf2(p, v, vl) __riscv_vse16_v_u16mf2(p, vreinterpret_v_bf16mf2_u16mf2(v), vl)
#define __riscv_vse16_v_bf16m1(p, v, vl) __riscv_vse16_v_u16m1(p, vreinterpret_v_bf16m1_u16m1(v), vl)
#define __riscv_vse16_v_bf16m2(p, v, vl) __riscv_vse16_v_u16m2(p, vreinterpret_v_bf16m2_u16m2(v), vl)
#define __riscv_vse16_v_bf16m4(p, v, vl) __riscv_vse16_v_u16m4(p, vreinterpret_v_bf16m4_u16m4(v), vl)
#define __riscv_vse16_v_bf16m8(p,v , vl) __riscv_vse16_v_u16m8(p, vreinterpret_v_bf16m8_u16m8(v), vl)
#endif

int main()
{
int return_value = 0;

size_t var_115 = 16u;
uint16_t var_114 [] = {2331u, 50321u, 25359u, 51650u, 44823u, 43544u, 34732u, 58047u};
size_t var_112 = 1u;
int8_t var_111 [] = {-16};
size_t var_109 = 2u;
uint16_t var_108 [] = {68u};
size_t var_106 = 2u;
uint16_t var_105 [] = {30389u};
size_t var_103 = 8u;
float64_t var_102 [] = {f64(4183473977970130944u)};
// 4.841275101341818e-29

size_t var_100 = 1u;
int8_t var_99 [] = {-108};
uint16_t var_97 [] = {65012u, 55190u, 46514u, 28875u, 36594u, 43727u, 7586u, 1367u};
int8_t var_96 [] = {-75, 96, 39, -9, -69, -46, 1, -22, 111, 11, -76, 59, -24, 24, -1, 89};
uint16_t var_94 [] = {7u, 14u, 13u, 0u, 6u, 14u, 2u, 8u};
int8_t var_92 [] = {-108, 37, -34};
int8_t var_91 [] = {125, 112, -29};
int8_t var_90 [] = {-115, -124, 14};
int8_t var_89 [] = {66};
float32_t var_88 [] = {f32(2346361110u), f32(2274765554u), f32(4170636865u), f32(1255137954u), f32(770048131u)};
// -8.421292075547953e-32, -2.2594732521906944e-34, -2.4479761673212445e+34, 6811985.0, 2.614819992474704e-11

float32_t var_87 [] = {f32(1690833471u), f32(1611751969u), f32(3039507657u), f32(1499035410u), f32(3666009633u)};
// 2.9525308674865006e+22, 4.190388786840365e+19, -6.377927661560534e-07, 3825686821208064.0, -1.841799014383616e+16

uint64_t var_86 [] = {3425713696681443193u, 5039499209058164190u, 6049277984953311932u, 8873752123271114414u, 9754200587838842769u};
float64_t var_85 [] = {f64(2476498982022245897u)};
// 3.8813899938074913e-143

uint16_t var_84 [] = {19327u, 36695u, 50674u, 17467u};
uint16_t var_83 [] = {22045u, 32601u, 55272u, 26559u};
uint16_t var_82 [] = {3815u};
uint8_t var_81 [] = {116u, 85u, 204u, 9u, 205u, 156u, 44u, 95u};
uint16_t var_80 [] = {125u, 11200u, 34490u, 18374u, 4588u, 35391u, 63495u, 10305u};
uint16_t var_79 [] = {15332u};
uint16_t var_78 [] = {1453u, 28298u, 2362u, 23807u, 62748u, 52974u, 59001u, 40770u};
uint16_t var_77 [] = {46274u, 10845u, 29681u, 59897u, 19443u, 17522u, 9033u, 21769u};
uint8_t var_76 [] = {51u, 70u, 115u, 250u, 97u, 187u, 167u, 45u};
int32_t var_75 [] = {228679613, 1651811947, -2100244616, -2082713800, 634254738, -390199277, 1417009460, -1348998711};
int32_t var_74 [] = {-1513452368, 349133221, -405006840, -217144520, 1959356346, -1224636336, -576064041, -1037028305};
int32_t var_73 [] = {1249999099, 991531407, -1993434062, -274053194, 1121453020, 2040089161, 170172760, 983679859};
uint16_t var_72 [] = {48382u, 12539u, 64301u, 45250u, 42097u, 26733u, 49161u, 59749u};
uint16_t var_71 [] = {8893u, 30107u, 19590u, 37346u, 19495u, 6196u, 41086u, 17234u};
uint16_t var_70 [] = {49157u, 58982u, 14107u, 53921u, 28458u, 3999u, 42377u, 16999u};
int8_t var_69 [] = {51, 63, 26, 58, 61, 59};
int8_t var_68 [] = {-16, -106, -123, 82, 33, -21};
int8_t var_67 [] = {40, -112, 39, -51, -120, -5};
int8_t var_66 [] = {3};
uint8_t var_65 [] = {249u, 156u, 104u, 248u, 181u, 207u, 205u, 28u};
uint16_t var_64 [] = {58047u, 8278u, 15783u, 9954u, 32465u, 59765u, 43410u, 64808u};
uint16_t var_63 [] = {53522u, 2331u, 50321u, 25359u, 51650u, 44823u, 43544u, 34732u};
int8_t var_62 [] = {-69, 99, -93, 118, 114, -30, 108, 104};
int8_t var_61 [] = {88, -96, 119, 5, -125, 104, 122, -42};
uint8_t var_60 = 45u;
float32_t var_59 = f32(276134744u);
// 4.841275101341818e-29

float32_t var_58 = f32(2996294259u);
// -1.7674414820589845e-08

uint64_t var_57 = 6032944606398802016u;
uint8_t var_56 = 242u;
int32_t var_55 = -1832850957;
__riscv_vsetvl_e16m1(8);
vuint16m1_t var_93 = __riscv_vle16_v_u16m1(var_94, 8);
// 7, 14, 13, 0, 6, 14, 2, 8

__riscv_vsetvl_e8mf2(3);
vint8mf2_t var_20 = __riscv_vle8_v_i8mf2(var_92, 3);
// -108, 37, -34

vint8mf2_t var_21 = __riscv_vle8_v_i8mf2(var_91, 3);
// 125, 112, -29

vint8mf2_t var_22 = __riscv_vle8_v_i8mf2(var_90, 3);
// -115, -124, 14

__riscv_vsetvl_e32m2(5);
vfloat32m2_t var_25 = __riscv_vle32_v_f32m2(var_88, 5);
// -8.421292075547953e-32, -2.2594732521906944e-34, -2.4479761673212445e+34, 6811985.0, 2.614819992474704e-11

vfloat32m2_t var_26 = __riscv_vle32_v_f32m2(var_87, 5);
// 2.9525308674865006e+22, 4.190388786840365e+19, -6.377927661560534e-07, 3825686821208064.0, -1.841799014383616e+16

vuint64m4_t var_27 = __riscv_vle64_v_u64m4(var_86, 5);
// 3425713696681443193, 5039499209058164190, 6049277984953311932, 8873752123271114414, 9754200587838842769

__riscv_vsetvl_e16m1(4);
vuint16m1_t var_29 = __riscv_vle16_v_u16m1(var_84, 4);
// 19327, 36695, 50674, 17467

vuint16m1_t var_30 = __riscv_vle16_v_u16m1(var_83, 4);
// 22045, 32601, 55272, 26559

__riscv_vsetvl_e8mf2(8);
vuint8mf2_t var_32 = __riscv_vle8_v_u8mf2(var_81, 8);
// 116, 85, 204, 9, 205, 156, 44, 95

vuint16m1_t var_33 = __riscv_vle16_v_u16m1(var_80, 8);
// 125, 11200, 34490, 18374, 4588, 35391, 63495, 10305

vuint16m1_t var_36 = __riscv_vle16_v_u16m1(var_78, 8);
// 1453, 28298, 2362, 23807, 62748, 52974, 59001, 40770

vuint16m1_t var_37 = __riscv_vle16_v_u16m1(var_77, 8);
// 46274, 10845, 29681, 59897, 19443, 17522, 9033, 21769

vuint8mf2_t var_38 = __riscv_vle8_v_u8mf2(var_76, 8);
// 51, 70, 115, 250, 97, 187, 167, 45

__riscv_vsetvl_e32m8(8);
vint32m8_t var_39 = __riscv_vle32_v_i32m8(var_75, 8);
// 228679613, 1651811947, -2100244616, -2082713800, 634254738, -390199277, 1417009460, -1348998711

__riscv_vsetvl_e32m2(8);
vint32m2_t var_40 = __riscv_vle32_v_i32m2(var_74, 8);
// -1513452368, 349133221, -405006840, -217144520, 1959356346, -1224636336, -576064041, -1037028305

vint32m2_t var_42 = __riscv_vle32_v_i32m2(var_73, 8);
// 1249999099, 991531407, -1993434062, -274053194, 1121453020, 2040089161, 170172760, 983679859

vuint16m1_t var_43 = __riscv_vle16_v_u16m1(var_72, 8);
// 48382, 12539, 64301, 45250, 42097, 26733, 49161, 59749

vuint16m1_t var_44 = __riscv_vle16_v_u16m1(var_71, 8);
// 8893, 30107, 19590, 37346, 19495, 6196, 41086, 17234

vuint16m1_t var_45 = __riscv_vle16_v_u16m1(var_70, 8);
// 49157, 58982, 14107, 53921, 28458, 3999, 42377, 16999

__riscv_vsetvl_e8m2(6);
vint8m2_t var_46 = __riscv_vle8_v_i8m2(var_69, 6);
// 51, 63, 26, 58, 61, 59

__riscv_vsetvl_e8mf2(6);
vint8mf2_t var_47 = __riscv_vle8_v_i8mf2(var_68, 6);
// -16, -106, -123, 82, 33, -21

vint8mf2_t var_48 = __riscv_vle8_v_i8mf2(var_67, 6);
// 40, -112, 39, -51, -120, -5

__riscv_vsetvl_e8mf2(8);
vuint8mf2_t var_50 = __riscv_vle8_v_u8mf2(var_65, 8);
// 249, 156, 104, 248, 181, 207, 205, 28

__riscv_vsetvl_e16m4(8);
vuint16m4_t var_51 = __riscv_vle16_v_u16m4(var_64, 8);
// 58047, 8278, 15783, 9954, 32465, 59765, 43410, 64808

__riscv_vsetvl_e16m1(8);
vuint16m1_t var_52 = __riscv_vle16_v_u16m1(var_63, 8);
// 53522, 2331, 50321, 25359, 51650, 44823, 43544, 34732

vint8mf2_t var_53 = __riscv_vle8_v_i8mf2(var_62, 8);
// -69, 99, -93, 118, 114, -30, 108, 104

vint8mf2_t var_54 = __riscv_vle8_v_i8mf2(var_61, 8);
// 88, -96, 119, 5, -125, 104, 122, -42

__riscv_vsetvl_e32m2(5);
vfloat32m2_t var_16 = __riscv_vfmv_v_f_f32m2(var_59, 5);
// 4.841275101341818e-29, 4.841275101341818e-29, 4.841275101341818e-29, 4.841275101341818e-29, 4.841275101341818e-29

vbool16_t var_24 = __riscv_vmsltu_vx_u64m4_b16(var_27, var_57, 5);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e8mf2(8);
vuint8mf2_t var_14 = __riscv_vmv_s_x_u8mf2_tu(var_32, var_60, 8);
// 45, 85, 204, 9, 205, 156, 44, 95

vbool16_t var_35 = __riscv_vmsbc_vx_u8mf2_b16(var_38, var_56, 8);
// 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32m8(8);
int32_t var_10 = __riscv_vmv_x_s_i32m8_i32(var_39);
// 228679613

__riscv_vsetvl_e32m2(8);
vbool16_t var_41 = __riscv_vmsge_vx_i32m2_b16(var_42, var_55, 8);
// 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e8m2(6);
int8_t var_7 = __riscv_vmv_x_s_i8m2_i8(var_46);
// 51

__riscv_vsetvl_e16m4(8);
uint16_t var_4 = __riscv_vmv_x_s_u16m4_u16(var_51);
// 58047

__riscv_vsetvl_e32m2(5);
vfloat32m2_t var_18 = __riscv_vfnmsac_vf_f32m2_tumu(var_24, var_25, var_58, var_26, 5);
// 521842552995840.0, 740626726912.0, -2.4479761673212445e+34, 6811985.0, 2.614819992474704e-11

__riscv_vsetvl_e8mf2(1);
vbool16_t var_5 = __riscv_vmadc_vv_u8mf2_b16(var_14, var_50, 1);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32m2(8);
vint32m2_t var_9 = __riscv_vsbc_vxm_i32m2(var_40, var_10, var_41, 8);
// -1742131982, 120453607, -633686453, -445824134, 1730676732, -1453315950, -804743655, -1265707919

vuint16m1_t var_2 = __riscv_vslide1down_vx_u16m1(var_52, var_4, 8);
// 2331, 50321, 25359, 51650, 44823, 43544, 34732, 58047

vbool16_t var_3 = __riscv_vmadc_vv_i32m2_b16(var_9, var_9, 8);
// 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vse16_v_u16m1(var_97, var_2, 8);
vint8mf2_t var_0 = __riscv_vloxei16_v_i8mf2_tumu(var_3, var_54, var_96, var_93, 8);
// -22, -96, 24, -75, -125, -1, 39, 111

if(!check(var_97, var_114, var_115)) {cerr << "check 113 fails" << endl; return_value = 1;}
__riscv_vsetvl_e8mf2(1);
vbool16_t var_1 = __riscv_vmsle_vv_i8mf2_b16_mu(var_3, var_5, var_0, var_53, 1);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vint8mf2_t var_6 = __riscv_vslide1down_vx_i8mf2_tumu(var_1, var_47, var_48, var_7, 1);
// -16, -106, -123, 82, 33, -21

vuint16m1_t var_8 = __riscv_vor_vv_u16m1_tumu(var_1, var_43, var_44, var_45, 1);
// 48382, 12539, 64301, 45250, 42097, 26733, 49161, 59749

vfloat32m2_t var_12 = __riscv_vfdiv_vv_f32m2_tumu(var_1, var_16, var_16, var_18, 1);
// 4.841275101341818e-29, 4.841275101341818e-29, 4.841275101341818e-29, 4.841275101341818e-29, 4.841275101341818e-29

vint8mf2_t var_19 = __riscv_vaadd_vv_i8mf2_tumu(var_1, var_20, var_21, var_22, 0, 1);
// -108, 37, -34

__riscv_vse8_v_i8mf2(var_66, var_6, 1);
vuint16m1_t var_11 = __riscv_vmacc_vv_u16m1_tumu(var_35, var_8, var_36, var_37, 1);
// 44568, 12539, 64301, 45250, 42097, 26733, 49161, 59749

vfloat64m4_t var_17 = __riscv_vfwcvt_f_f_v_f64m4(var_12, 1);
// 4.841275101341818e-29

__riscv_vse8_v_i8mf2(var_89, var_19, 1);
if(!check(var_66, var_111, var_112)) {cerr << "check 110 fails" << endl; return_value = 1;}
vuint16m1_t var_13 = __riscv_vremu_vv_u16m1(var_11, var_33, 1);
// 68

vuint16m1_t var_15 = __riscv_vnmsac_vv_u16m1(var_11, var_29, var_30, 1);
// 30389, 12539, 64301, 45250, 42097, 26733, 49161, 59749

__riscv_vse64_v_f64m4(var_85, var_17, 1);
if(!check(var_89, var_99, var_100)) {cerr << "check 98 fails" << endl; return_value = 1;}
__riscv_vse16_v_u16m1(var_79, var_13, 1);
__riscv_vse16_v_u16m1(var_82, var_15, 1);
if(!check(var_85, var_102, var_103)) {cerr << "check 101 fails" << endl; return_value = 1;}
if(!check(var_79, var_108, var_109)) {cerr << "check 107 fails" << endl; return_value = 1;}
if(!check(var_82, var_105, var_106)) {cerr << "check 104 fails" << endl; return_value = 1;}
if (return_value)
  __builtin_abort ();
return return_value;
}
