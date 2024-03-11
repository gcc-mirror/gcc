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

size_t var_115 = 1u;
uint8_t var_114 [] = {10u};
size_t var_112 = 4u;
uint32_t var_111 [] = {3132511239u};
size_t var_109 = 74u;
int16_t var_108 [] = {2858, 492, 24911, -11303, -31616, 28533, 19034, -9653, 28885, 7610, 12944, -20025, 1686, 30217, -1395, -3672, 2306, -18824, -31159, -18821, 28140, 6059, 21475, -8012, -9400, 2811, 28276, 21990, 28916, -10924, 5167, 20908, -22863, -21651, 18443, -18027, -29506};
size_t var_106 = 4u;
float32_t var_105 [] = {f32(3922195668u)};
// -3.021655077935938e+25

uint16_t var_103 = 0u;
int8_t var_101 [] = {96, 34};
int32_t var_100 [] = {1405457466, 1721118922};
float32_t var_99 [] = {f32(3922195668u)};
// -3.021655077935938e+25

float32_t var_98 [] = {f32(967328861u), f32(2167036240u)};
// 0.0003209439164493233, -3.1288147269893657e-38

int16_t var_97 [] = {-1123, -6424};
int16_t var_96 [] = {9778, 13362};
int16_t var_95 [] = {25985, 12965, 24364, 1620, 11788, 17666, 9112, -16259, 30211, -22913, -25451, 24681, 21665, 14991, -1339, 848, 5275, -13754, 7902, -13065, -1428, -10034, -11668, -20846, 5887, -22465, -6031, 30732, 7605, -10446, 1100, -11314, 30319, 16023, 25829, -8192, 19538};
int16_t var_94 [] = {17771, 2858, 492, 24911, -11303, -31616, 28533, 19034, -9653, 28885, 7610, 12944, -20025, 1686, 30217, -1395, -3672, 2306, -18824, -31159, -18821, 28140, 6059, 21475, -8012, -9400, 2811, 28276, 21990, 28916, -10924, 5167, 20908, -22863, -21651, 18443, -18027};
int16_t var_93 [] = {-21330, -22858, 24008, 17967, -21231, -32167, 18812, 20756, -1518, 18916, -27871, 30718, 27000, 2704, 6947, 27603, 12463, -19003, 530, -30671, -24051, 21005, 29973, -3287, 1940, 31175, -32755, 22027, 19472, -5055, -8470, -18339, 17283, 15399, -19484, 15629, 28507};
uint16_t var_92 [] = {63787u, 42257u, 24150u, 52956u, 61108u, 46587u, 65242u, 48126u, 64523u, 61995u, 30399u, 12749u, 54004u, 52568u, 33099u, 41297u, 23093u, 60951u, 25737u, 12347u, 23498u, 51555u, 60233u, 61151u, 56996u, 53763u, 17372u, 46632u, 57944u, 17207u, 20813u, 59048u, 29548u, 61811u, 50413u, 63404u, 21680u};
int16_t var_91 [] = {-6131, 29574};
int32_t var_90 [] = {1644247083, -1282916520};
int32_t var_89 [] = {1227673631, 857229023};
uint32_t var_88 [] = {3132511239u};
uint32_t var_87 [] = {4262063875u};
uint16_t var_86 [] = {40919u};
uint32_t var_85 [] = {1286225402u};
int16_t var_84 [] = {-26820, -27331};
float64_t var_83 [] = {f64(11069717378845155319u), f64(13721794338725332728u)};
// -2.899173043426029e-185, -5.514630093567779e-08

float64_t var_82 [] = {f64(2718112256676496315u), f64(915798642043068280u)};
// 5.695500157055262e-127, 1.9281508973710386e-247

uint64_t var_81 [] = {2807378651874114280u, 3677046095772117620u};
uint32_t var_80 [] = {3179970750u, 3445021534u};
int16_t var_79 [] = {-3759, 22117};
int64_t var_78 [] = {4276742551513081964, 1244361136627792199};
int64_t var_77 [] = {-738627246414404434, 1483649367620429950};
uint32_t var_76 [] = {666318773u, 3789337898u};
uint32_t var_75 [] = {3315562468u, 1268900688u};
uint32_t var_74 [] = {726843747u, 1886542783u};
uint8_t var_73 [] = {35u};
uint8_t var_72 [] = {26u};
uint8_t var_71 [] = {71u, 246u};
uint8_t var_70 [] = {254u, 78u};
uint16_t var_69 [] = {26491u};
uint8_t var_68 [] = {205u};
uint8_t var_67 [] = {61u};
uint8_t var_66 [] = {54u};
size_t var_65 = 0u;
int8_t var_64 = -34;
int8_t var_63 = 105;
uint16_t var_62 = 16597u;
uint64_t var_61 = 8406458174123303558u;
uint32_t var_60 = 2871496165u;
__riscv_vsetvl_e8mf8(2);
vint8mf8_t var_20 = __riscv_vle8_v_i8mf8(var_101, 2);
// 96, 34

__riscv_vsetvl_e32m1(2);
vint32m1_t var_21 = __riscv_vle32_v_i32m1(var_100, 2);
// 1405457466, 1721118922

vfloat32m1_t var_24 = __riscv_vle32_v_f32m1(var_98, 2);
// 0.0003209439164493233, -3.1288147269893657e-38

vint16mf2_t var_25 = __riscv_vle16_v_i16mf2(var_97, 2);
// -1123, -6424

vint16mf2_t var_26 = __riscv_vle16_v_i16mf2(var_96, 2);
// 9778, 13362

__riscv_vsetvl_e16m8(37);
vint16m8_t var_28 = __riscv_vle16_v_i16m8(var_95, 37);
// 25985, 12965, 24364, 1620, 11788, 17666, 9112, -16259, 30211, -22913, -25451, 24681, 21665, 14991, -1339, 848, 5275, -13754, 7902, -13065, -1428, -10034, -11668, -20846, 5887, -22465, -6031, 30732, 7605, -10446, 1100, -11314, 30319, 16023, 25829, -8192, 19538

vint16m8_t var_29 = __riscv_vle16_v_i16m8(var_94, 37);
// 17771, 2858, 492, 24911, -11303, -31616, 28533, 19034, -9653, 28885, 7610, 12944, -20025, 1686, 30217, -1395, -3672, 2306, -18824, -31159, -18821, 28140, 6059, 21475, -8012, -9400, 2811, 28276, 21990, 28916, -10924, 5167, 20908, -22863, -21651, 18443, -18027

vuint16m8_t var_31 = __riscv_vle16_v_u16m8(var_92, 37);
// 63787, 42257, 24150, 52956, 61108, 46587, 65242, 48126, 64523, 61995, 30399, 12749, 54004, 52568, 33099, 41297, 23093, 60951, 25737, 12347, 23498, 51555, 60233, 61151, 56996, 53763, 17372, 46632, 57944, 17207, 20813, 59048, 29548, 61811, 50413, 63404, 21680

__riscv_vsetvl_e16mf4(2);
vint16mf4_t var_32 = __riscv_vle16_v_i16mf4(var_91, 2);
// -6131, 29574

__riscv_vsetvl_e32m1(2);
vint32m1_t var_33 = __riscv_vle32_v_i32m1(var_90, 2);
// 1644247083, -1282916520

vint32m1_t var_34 = __riscv_vle32_v_i32m1(var_89, 2);
// 1227673631, 857229023

__riscv_vsetvl_e32mf2(1);
vuint32mf2_t var_35 = __riscv_vle32_v_u32mf2(var_88, 1);
// 3132511239

vuint32mf2_t var_36 = __riscv_vle32_v_u32mf2(var_87, 1);
// 4262063875

vuint16mf4_t var_37 = __riscv_vle16_v_u16mf4(var_86, 1);
// 40919

__riscv_vsetvl_e16mf4(2);
vint16mf4_t var_39 = __riscv_vle16_v_i16mf4(var_84, 2);
// -26820, -27331

vfloat64m1_t var_41 = __riscv_vle64_v_f64m1(var_83, 2);
// -2.899173043426029e-185, -5.514630093567779e-08

vfloat64m1_t var_42 = __riscv_vle64_v_f64m1(var_82, 2);
// 5.695500157055262e-127, 1.9281508973710386e-247

vuint64m1_t var_43 = __riscv_vle64_v_u64m1(var_81, 2);
// 2807378651874114280, 3677046095772117620

__riscv_vsetvl_e32m4(2);
vuint32m4_t var_44 = __riscv_vle32_v_u32m4(var_80, 2);
// 3179970750, 3445021534

__riscv_vsetvl_e16mf4(2);
vint16mf4_t var_45 = __riscv_vle16_v_i16mf4(var_79, 2);
// -3759, 22117

vint64m1_t var_47 = __riscv_vle64_v_i64m1(var_78, 2);
// 4276742551513081964, 1244361136627792199

vint64m1_t var_48 = __riscv_vle64_v_i64m1(var_77, 2);
// -738627246414404434, 1483649367620429950

vuint32mf2_t var_49 = __riscv_vle32_v_u32mf2(var_76, 2);
// 666318773, 3789337898

vuint32mf2_t var_50 = __riscv_vle32_v_u32mf2(var_75, 2);
// 3315562468, 1268900688

vuint32mf2_t var_51 = __riscv_vle32_v_u32mf2(var_74, 2);
// 726843747, 1886542783

__riscv_vsetvl_e8mf8(1);
vuint8mf8_t var_52 = __riscv_vle8_v_u8mf8(var_73, 1);
// 35

__riscv_vsetvl_e8mf8(2);
vuint8mf8_t var_54 = __riscv_vle8_v_u8mf8(var_71, 2);
// 71, 246

vuint8mf8_t var_55 = __riscv_vle8_v_u8mf8(var_70, 2);
// 254, 78

__riscv_vsetvl_e16mf4(1);
vuint16mf4_t var_56 = __riscv_vle16_v_u16mf4(var_69, 1);
// 26491

vuint8mf8_t var_57 = __riscv_vle8_v_u8mf8(var_68, 1);
// 205

vuint8mf8_t var_58 = __riscv_vle8_v_u8mf8(var_67, 1);
// 61

vuint8mf8_t var_59 = __riscv_vle8_v_u8mf8(var_66, 1);
// 54

__riscv_vsetvl_e8mf8(2);
vint8mf8_t var_19 = __riscv_vsmul_vx_i8mf8(var_20, var_63, 0,2);
// 79, 28

__riscv_vsetvl_e16mf2(2);
vbool32_t var_22 = __riscv_vmsne_vv_i16mf2_b32(var_25, var_26, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e16m8(37);
vbool2_t var_27 = __riscv_vmsne_vx_u16m8_b2(var_31, var_62, 37);
// 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e16mf4(2);
int16_t var_16 = __riscv_vmv_x_s_i16mf4_i16(var_32);
// -6131

__riscv_vsetvl_e16mf4(1);
vuint16mf4_t var_102 = __riscv_vminu_vx_u16mf4(var_37, var_103, 1);
// 0

__riscv_vsetvl_e64m1(2);
vbool64_t var_40 = __riscv_vmsbc_vx_u64m1_b64(var_43, var_61, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32m4(2);
uint32_t var_10 = __riscv_vmv_x_s_u32m4_u32(var_44);
// 3179970750

__riscv_vsetvl_e32mf2(2);
vbool64_t var_46 = __riscv_vmseq_vx_u32mf2_b64(var_49, var_60, 2);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vuint32mf2_t var_7 = __riscv_vor_vv_u32mf2(var_50, var_51, 2);
// 4024432615, 2079587327

__riscv_vsetvl_e8mf8(1);
vuint16mf4_t var_3 = __riscv_vwsubu_wv_u16mf4(var_56, var_57, 1);
// 26286

__riscv_vsetvl_e16mf4(2);
vint16mf4_t var_9 = __riscv_vadd_vx_i16mf4(var_45, var_16, 2);
// -9890, 15986

vbool64_t var_6 = __riscv_vmadc_vx_u32mf2_b64(var_7, var_10, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e8mf8(1);
vuint16mf4_t var_2 = __riscv_vwsubu_wv_u16mf4(var_3, var_58, 1);
// 26225

__riscv_vsetvl_e16mf4(2);
vbool64_t var_0 = __riscv_vmsne_vx_i16mf4_b64(var_9, var_16, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e8mf8(1);
vuint8mf8_t var_1 = __riscv_vnclipu_wx_u8mf8_mu(var_0, var_59, var_2, var_65, 0, 1);
// 255

__riscv_vsetvl_e8mf8(2);
vbool64_t var_4 = __riscv_vmsgtu_vv_u8mf8_b64_mu(var_0, var_6, var_54, var_55, 2);
// 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e8mf8(1);
vuint8mf8_t var_5 = __riscv_vremu_vv_u8mf8(var_1, var_52, 1);
// 10

__riscv_vsetvl_e64m1(2);
vbool64_t var_8 = __riscv_vmseq_vv_i64m1_b64_mu(var_4, var_46, var_47, var_48, 2);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32mf2(1);
vuint32mf2_t var_13 = __riscv_vrgatherei16_vv_u32mf2_mu(var_4, var_35, var_36, var_102, 1);
// 3132511239

__riscv_vse8_v_u8mf8(var_72, var_5, 1);
__riscv_vsetvl_e64m1(2);
vbool64_t var_11 = __riscv_vmfge_vv_f64m1_b64_mu(var_8, var_40, var_41, var_42, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32mf2(1);
__riscv_vse32_v_u32mf2(var_85, var_13, 1);
if(!check(var_72, var_114, var_115)) {cerr << "check 113 fails" << endl; return_value = 1;}
__riscv_vsetvl_e8mf8(2);
vint16mf4_t var_12 = __riscv_vwmacc_vx_i16mf4_mu(var_11, var_39, var_64, var_19, 2);
// -29506, -28283

if(!check(var_85, var_111, var_112)) {cerr << "check 110 fails" << endl; return_value = 1;}
vint32m1_t var_14 = __riscv_vwredsum_vs_i16mf4_i32m1_tu(var_33, var_12, var_34, 2);
// 1227615842

int16_t var_15 = __riscv_vmv_x_s_i16mf4_i16(var_12);
// -29506

__riscv_vsetvl_e32m1(1);
vbool32_t var_18 = __riscv_vmadc_vvm_i32m1_b32(var_14, var_21, var_22, 1);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e16m8(37);
vint16m8_t var_17 = __riscv_vslide1down_vx_i16m8_mu(var_27, var_28, var_29, var_15, 37);
// 2858, 492, 24911, -11303, -31616, 28533, 19034, -9653, 28885, 7610, 12944, -20025, 1686, 30217, -1395, -3672, 2306, -18824, -31159, -18821, 28140, 6059, 21475, -8012, -9400, 2811, 28276, 21990, 28916, -10924, 5167, 20908, -22863, -21651, 18443, -18027, -29506

__riscv_vsetvl_e32m1(1);
__riscv_vse32_v_f32m1_m(var_18, var_99, var_24, 1);
__riscv_vsetvl_e16m8(37);
__riscv_vse16_v_i16m8(var_93, var_17, 37);
if(!check(var_99, var_105, var_106)) {cerr << "check 104 fails" << endl; return_value = 1;}
if(!check(var_93, var_108, var_109)) {cerr << "check 107 fails" << endl; return_value = 1;}
if (return_value)
  __builtin_abort ();
return return_value;
}
