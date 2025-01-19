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

size_t var_140 = 3u;
uint8_t var_139 [] = {105u, 153u, 228u};
size_t var_137 = 4u;
int32_t var_136 [] = {825454882};
size_t var_134 = 4u;
int32_t var_133 [] = {-2044580921};
size_t var_131 = 8u;
float64_t var_130 [] = {f64(11610980516175879577u)};
// -4.518691795485215e-149

size_t var_128 = 4u;
int32_t var_127 [] = {-29825};
size_t var_125 = 8u;
float64_t var_124 [] = {f64(4423419669832571904u)};
// 5.440929296973346e-13

uint8_t var_122 [] = {221u, 228u, 90u};
int32_t var_121 [] = {-1322184999, 1039089429, 1602706274, -1987162703, -745134703, -1874569310, 88319479, -1372648636};
uint8_t var_119 [] = {0u};
uint32_t var_117 = 0u;
float64_t var_115 [] = {f64(10570161852179085863u), f64(7905499064187989320u)};
// -1.1868637773690528e-218, 3.1028377645298396e+220

uint64_t var_114 [] = {14282878292220054692u, 11725909568683513054u};
uint64_t var_113 [] = {16091328503071571719u, 14182469813088437602u};
float32_t var_112 [] = {f32(363500320u), f32(527024411u), f32(1060187064u)};
// 6.889457551179815e-26, 4.949940227749096e-20, 0.6920733451843262

float32_t var_111 [] = {f32(3571833023u), f32(366867540u), f32(1264612092u)};
// -7897471254528.0, 8.964667095928483e-26, 14709500.0

float64_t var_110 [] = {f64(1878533856182074005u)};
// 4.210988290683248e-183

uint32_t var_109 [] = {2889287184u, 3870295574u, 3944299019u};
uint32_t var_108 [] = {423086972u, 2782331753u, 199732161u};
int32_t var_107 [] = {-278680732};
int16_t var_106 [] = {-29825};
int32_t var_105 [] = {-1263631215};
float64_t var_104 [] = {f64(12567359566196420376u)};
// -3.897704423428432e-85

float64_t var_103 [] = {f64(8479803601933450372u)};
// 7.282711776288908e+258

int16_t var_102 [] = {13510};
int16_t var_101 [] = {22815};
float64_t var_100 [] = {f64(13182948881270037331u), f64(8516350855071575190u), f64(2786864662567483237u)};
// -5.392986098814974e-44, 1.9872606665059677e+261, 2.1883326855203212e-122

float64_t var_99 [] = {f64(3815753008405595505u), f64(5017648848798215353u), f64(10680612954347991468u)};
// 1.3231952293488927e-53, 2.82709922599264e+27, -2.9894357005269383e-211

uint32_t var_98 [] = {3554357257u, 1304976622u, 1020453920u};
uint32_t var_97 [] = {1914414134u, 1757004891u, 4176597456u};
uint16_t var_96 [] = {58413u, 59159u, 11092u};
int32_t var_95 [] = {-1925946840};
float32_t var_94 [] = {f32(2592256229u), f32(3465161337u)};
// -5.405252834469027e-23, -1159019648.0

float32_t var_93 [] = {f32(1448849534u), f32(3554289590u)};
// 60387768664064.0, -1873938219008.0

float32_t var_92 [] = {f32(2432338206u), f32(2327076818u)};
// -9.88188756304636e-29, -1.736941309501721e-32

int16_t var_91 [] = {-14447, -24030, -944};
float32_t var_90 [] = {f32(1705242637u), f32(3515957841u)};
// 9.677173650427062e+22, -77985357824.0

int32_t var_89 [] = {-1071133475, 1791068978, 1264864376};
uint8_t var_88 [] = {227u, 218u, 120u};
int32_t var_87 [] = {-682915521, 1158910892, -1803838961};
uint32_t var_86 [] = {4105487819u, 851245870u, 3261314290u};
int32_t var_85 [] = {-757670017};
uint64_t var_84 [] = {14141995822691574791u, 11766039677310203423u, 13825515671073065986u};
uint64_t var_83 [] = {17556801802550960644u, 7380202866067124031u, 12435392562170195108u};
int32_t var_82 [] = {825438420, -2138872628, 712270620};
float32_t var_81 [] = {f32(522594453u), f32(2397720240u)};
// 3.5185435980521485e-20, -5.7761227000535974e-30

float32_t var_80 [] = {f32(3772693313u), f32(2321727347u)};
// -1.283959453225656e+20, -1.0917511781805477e-32

uint8_t var_79 [] = {58u, 226u, 70u};
uint8_t var_78 [] = {253u, 143u, 231u};
uint32_t var_77 [] = {2855396830u, 2505767773u, 1376872794u};
uint8_t var_76 [] = {105u, 153u, 228u};
float32_t var_75 = f32(2249259186u);
// -2.726610230227034e-35

int16_t var_74 = 31333;
int32_t var_73 = -2044580921;
size_t var_72 = 1u;
float64_t var_71 = f64(12489530239997367570u);
// -2.4207875634387096e-90

float64_t var_70 = f64(11610980516175879577u);
// -4.518691795485215e-149

float64_t var_69 = f64(16641254658498943323u);
// -7.663586581314764e+187

uint16_t var_68 = 53763u;
uint8_t var_67 = 20u;
__riscv_vsetvl_e8mf4(1);
vuint8mf4_t var_118 = __riscv_vle8_v_u8mf4(var_119, 1);
// 0

__riscv_vsetvl_e64m1(2);
vfloat64m1_t var_22 = __riscv_vle64_v_f64m1(var_115, 2);
// -1.1868637773690528e-218, 3.1028377645298396e+220

vuint64m1_t var_23 = __riscv_vle64_v_u64m1(var_114, 2);
// 14282878292220054692, 11725909568683513054

vuint64m1_t var_24 = __riscv_vle64_v_u64m1(var_113, 2);
// 16091328503071571719, 14182469813088437602

__riscv_vsetvl_e32m1(3);
vfloat32m1_t var_26 = __riscv_vle32_v_f32m1(var_112, 3);
// 6.889457551179815e-26, 4.949940227749096e-20, 0.6920733451843262

vfloat32m1_t var_27 = __riscv_vle32_v_f32m1(var_111, 3);
// -7897471254528.0, 8.964667095928483e-26, 14709500.0

vuint32m1_t var_29 = __riscv_vle32_v_u32m1(var_109, 3);
// 2889287184, 3870295574, 3944299019

vuint32m1_t var_30 = __riscv_vle32_v_u32m1(var_108, 3);
// 423086972, 2782331753, 199732161

__riscv_vsetvl_e32m1(1);
vint32m1_t var_31 = __riscv_vle32_v_i32m1(var_107, 1);
// -278680732

vint16mf2_t var_32 = __riscv_vle16_v_i16mf2(var_106, 1);
// -29825

vfloat64m2_t var_35 = __riscv_vle64_v_f64m2(var_104, 1);
// -3.897704423428432e-85

vint16mf2_t var_37 = __riscv_vle16_v_i16mf2(var_102, 1);
// 13510

vint16mf2_t var_38 = __riscv_vle16_v_i16mf2(var_101, 1);
// 22815

__riscv_vsetvl_e64m2(3);
vfloat64m2_t var_39 = __riscv_vle64_v_f64m2(var_100, 3);
// -5.392986098814974e-44, 1.9872606665059677e+261, 2.1883326855203212e-122

vfloat64m2_t var_40 = __riscv_vle64_v_f64m2(var_99, 3);
// 1.3231952293488927e-53, 2.82709922599264e+27, -2.9894357005269383e-211

__riscv_vsetvl_e32m2(3);
vuint32m2_t var_42 = __riscv_vle32_v_u32m2(var_98, 3);
// 3554357257, 1304976622, 1020453920

vuint32m2_t var_43 = __riscv_vle32_v_u32m2(var_97, 3);
// 1914414134, 1757004891, 4176597456

vuint16m1_t var_44 = __riscv_vle16_v_u16m1(var_96, 3);
// 58413, 59159, 11092

__riscv_vsetvl_e32mf2(2);
vfloat32mf2_t var_46 = __riscv_vle32_v_f32mf2(var_94, 2);
// -5.405252834469027e-23, -1159019648.0

vfloat32mf2_t var_47 = __riscv_vle32_v_f32mf2(var_93, 2);
// 60387768664064.0, -1873938219008.0

vfloat32mf2_t var_48 = __riscv_vle32_v_f32mf2(var_92, 2);
// -9.88188756304636e-29, -1.736941309501721e-32

__riscv_vsetvl_e16mf2(3);
vint16mf2_t var_49 = __riscv_vle16_v_i16mf2(var_91, 3);
// -14447, -24030, -944

__riscv_vsetvl_e32mf2(2);
vfloat32mf2_t var_50 = __riscv_vle32_v_f32mf2(var_90, 2);
// 9.677173650427062e+22, -77985357824.0

__riscv_vsetvl_e32m1(3);
vint32m1_t var_51 = __riscv_vle32_v_i32m1(var_89, 3);
// -1071133475, 1791068978, 1264864376

vuint8mf4_t var_53 = __riscv_vle8_v_u8mf4(var_88, 3);
// 227, 218, 120

vint32m1_t var_54 = __riscv_vle32_v_i32m1(var_87, 3);
// -682915521, 1158910892, -1803838961

vuint32m1_t var_56 = __riscv_vle32_v_u32m1(var_86, 3);
// 4105487819, 851245870, 3261314290

vuint64m2_t var_58 = __riscv_vle64_v_u64m2(var_84, 3);
// 14141995822691574791, 11766039677310203423, 13825515671073065986

vuint64m2_t var_59 = __riscv_vle64_v_u64m2(var_83, 3);
// 17556801802550960644, 7380202866067124031, 12435392562170195108

vint32m1_t var_60 = __riscv_vle32_v_i32m1(var_82, 3);
// 825438420, -2138872628, 712270620

__riscv_vsetvl_e32m1(2);
vfloat32m1_t var_61 = __riscv_vle32_v_f32m1(var_81, 2);
// 3.5185435980521485e-20, -5.7761227000535974e-30

vfloat32m1_t var_62 = __riscv_vle32_v_f32m1(var_80, 2);
// -1.283959453225656e+20, -1.0917511781805477e-32

__riscv_vsetvl_e8mf4(3);
vuint8mf4_t var_63 = __riscv_vle8_v_u8mf4(var_79, 3);
// 58, 226, 70

vuint8mf4_t var_64 = __riscv_vle8_v_u8mf4(var_78, 3);
// 253, 143, 231

vuint32m1_t var_65 = __riscv_vle32_v_u32m1(var_77, 3);
// 2855396830, 2505767773, 1376872794

vuint8mf4_t var_66 = __riscv_vle8_v_u8mf4(var_76, 3);
// 105, 153, 228

__riscv_vsetvl_e64m1(2);
vbool64_t var_21 = __riscv_vmfge_vf_f64m1_b64(var_22, var_69, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vbool64_t var_20 = __riscv_vmsne_vv_u64m1_b64(var_23, var_24, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32m1(3);
vbool32_t var_25 = __riscv_vmsgtu_vv_u32m1_b32(var_29, var_30, 3);
// 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e16mf2(1);
vbool32_t var_34 = __riscv_vmsne_vv_i16mf2_b32(var_37, var_38, 1);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e16m1(3);
vbool16_t var_41 = __riscv_vmsne_vx_u16m1_b16(var_44, var_68, 3);
// 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32mf2(2);
vfloat32mf2_t var_11 = __riscv_vfnmadd_vv_f32mf2(var_46, var_47, var_48, 2);
// 3.2641116476384013e-09, -2.171931148306111e+21

__riscv_vsetvl_e16mf2(3);
vint32m1_t var_10 = __riscv_vwadd_vx_i32m1(var_49, var_74, 3);
// 16886, 7303, 30389

vbool32_t var_52 = __riscv_vmsltu_vx_u8mf4_b32(var_53, var_67, 3);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32m1(1);
vuint32m1_t var_116 = __riscv_vminu_vx_u32m1(var_56, var_117, 1);
// 0

__riscv_vsetvl_e64m2(3);
vbool32_t var_55 = __riscv_vmsgeu_vv_u64m2_b32(var_58, var_59, 3);
// 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vse8_v_u8mf4(var_122, var_66, 3);
__riscv_vsetvl_e8mf8(2);
vbool64_t var_19 = __riscv_vmxor_mm_b64(var_20, var_21, 2);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32m2(3);
vuint32m2_t var_14 = __riscv_vsll_vx_u32m2_mu(var_41, var_42, var_43, var_72, 3);
// 3828828268, 3514009782, 4058227616

if(!check(var_122, var_139, var_140)) {cerr << "check 138 fails" << endl; return_value = 1;}
__riscv_vsetvl_e32mf2(2);
vfloat32mf2_t var_9 = __riscv_vfsgnjx_vf_f32mf2_mu(var_19, var_11, var_50, var_75, 2);
// 3.2641116476384013e-09, -2.171931148306111e+21

__riscv_vsetvl_e32m2(3);
uint32_t var_13 = __riscv_vmv_x_s_u32m2_u32(var_14);
// 3828828268

__riscv_vsetvl_e32mf2(2);
vfloat32m1_t var_4 = __riscv_vfredusum_vs_f32mf2_f32m1_tu(var_61, var_9, var_62, 2);
// -2.3003270848325836e+21

__riscv_vsetvl_e32m1(1);
vbool32_t var_3 = __riscv_vmfgt_vv_f32m1_b32(var_4, var_4, 1);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vbool32_t var_2 = __riscv_vmsne_vv_u8mf4_b32_mu(var_3, var_3, var_63, var_64, 1);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vbool32_t var_1 = __riscv_vmsleu_vx_u32m1_b32_mu(var_2, var_2, var_65, var_13, 1);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vint32m1_t var_0 = __riscv_vloxei8_v_i32m1_mu(var_1, var_10, var_121, var_118, 1);
// 16886, 7303, 30389

vint32m1_t var_5 = __riscv_vredxor_vs_i32m1_i32m1_tu(var_0, var_10, var_60, 1);
// 825454882

vint32m1_t var_12 = __riscv_vmv_s_x_i32m1_tu(var_0, var_73, 1);
// -2044580921, 7303, 30389

vint32m1_t var_6 = __riscv_vrgather_vv_i32m1_mu(var_55, var_5, var_10, var_116, 1);
// 825454882, 7303, 30389

vint32m1_t var_7 = __riscv_vnmsub_vv_i32m1(var_5, var_10, var_54, 1);
// -2145177453, 7303, 30389

__riscv_vse32_v_i32m1(var_95, var_12, 1);
__riscv_vse32_v_i32m1(var_85, var_6, 1);
vbool32_t var_8 = __riscv_vmadc_vvm_i32m1_b32(var_7, var_51, var_52, 1);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

if(!check(var_95, var_133, var_134)) {cerr << "check 132 fails" << endl; return_value = 1;}
if(!check(var_85, var_136, var_137)) {cerr << "check 135 fails" << endl; return_value = 1;}
vfloat64m2_t var_15 = __riscv_vfmacc_vf_f64m2_mu(var_8, var_39, var_71, var_40, 1);
// -5.392986098814974e-44, 1.9872606665059677e+261, 2.1883326855203212e-122

vint32m1_t var_17 = __riscv_vsext_vf2_i32m1_mu(var_8, var_31, var_32, 1);
// -29825

vfloat64m2_t var_16 = __riscv_vfslide1down_vf_f64m2_mu(var_34, var_15, var_35, var_70, 1);
// -4.518691795485215e-149, 1.9872606665059677e+261, 2.1883326855203212e-122

vfloat64m2_t var_18 = __riscv_vfwnmsac_vv_f64m2_mu(var_25, var_15, var_26, var_27, 1);
// 5.440929296973346e-13, 1.9872606665059677e+261, 2.1883326855203212e-122

__riscv_vse32_v_i32m1(var_105, var_17, 1);
__riscv_vse64_v_f64m2(var_103, var_16, 1);
__riscv_vse64_v_f64m2(var_110, var_18, 1);
if(!check(var_105, var_127, var_128)) {cerr << "check 126 fails" << endl; return_value = 1;}
if(!check(var_103, var_130, var_131)) {cerr << "check 129 fails" << endl; return_value = 1;}
if(!check(var_110, var_124, var_125)) {cerr << "check 123 fails" << endl; return_value = 1;}
if (return_value)
  __builtin_abort ();
return return_value;
}
