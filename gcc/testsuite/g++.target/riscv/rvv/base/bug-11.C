/* { dg-do run { target { riscv_vector } } } */
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

size_t var_147 = 1u;
uint8_t var_146 [] = {150u};
size_t var_144 = 1u;
int8_t var_143 [] = {5};
size_t var_141 = 8u;
uint64_t var_140 [] = {2157312243695362217u};
size_t var_138 = 1u;
int8_t var_137 [] = {5};
size_t var_135 = 2u;
int16_t var_134 [] = {28606};
size_t var_132 = 1u;
int8_t var_131 [] = {-3};
int8_t var_129 [] = {-87};
int16_t var_128 [] = {23919};
int16_t var_127 [] = {-24506};
int16_t var_126 [] = {20994};
uint8_t var_125 [] = {7u};
uint8_t var_124 [] = {19u};
int8_t var_123 [] = {127, -119};
int8_t var_122 [] = {-45, 89};
int8_t var_121 [] = {84};
uint32_t var_120 [] = {3102867433u, 3759812467u};
uint32_t var_119 [] = {3244721859u, 17611546u};
int8_t var_118 [] = {-54, 20};
int8_t var_117 [] = {0, 121};
uint16_t var_116 [] = {48601u, 39502u};
uint16_t var_115 [] = {55110u, 60687u};
int16_t var_114 [] = {-22328};
int16_t var_113 [] = {28601};
float32_t var_112 [] = {f32(2923498028u)};
// -4.3892153800406675e-11

int8_t var_111 [] = {69, 42};
int8_t var_110 [] = {89, 1};
uint8_t var_109 [] = {78u, 68u};
int8_t var_108 [] = {-76, 47};
int8_t var_107 [] = {12, 94};
uint64_t var_106 [] = {18400157712069531911u, 5717040025913777552u};
uint64_t var_105 [] = {13184553937407245441u, 14207308214616682331u};
uint64_t var_104 [] = {2157312243695362217u, 14090007358265115459u};
uint64_t var_103 [] = {5599154151314945157u};
uint8_t var_102 [] = {45u, 70u};
uint8_t var_101 [] = {183u, 68u};
uint8_t var_100 [] = {95u, 83u};
int8_t var_99 [] = {-112, -63};
int8_t var_98 [] = {60};
uint16_t var_97 [] = {27441u};
uint8_t var_96 [] = {86u};
uint8_t var_95 [] = {218u};
int32_t var_94 [] = {1349881114};
int32_t var_93 [] = {-1370289438};
int16_t var_92 [] = {-28549, -886};
uint8_t var_91 [] = {24u, 242u};
uint8_t var_90 [] = {96u, 57u};
uint8_t var_89 [] = {109u};
uint8_t var_88 [] = {150u};
float32_t var_87 [] = {f32(482885409u), f32(825021819u)};
// 1.3251208240642717e-21, 2.5150643789828564e-09

float32_t var_86 [] = {f32(3826107470u), f32(1715869719u)};
// -1.0460544631490656e+22, 2.338682356976554e+23

size_t var_85 = 0u;
size_t var_84 = 0u;
size_t var_83 = 1u;
uint8_t var_82 = 75u;
int8_t var_81 = 5;
size_t var_80 = 0u;
int8_t var_79 = -90;
int8_t var_78 = -29;
float32_t var_77 = f32(3828347624u);
// -1.2982733811403788e+22

uint8_t var_76 = 110u;
uint16_t var_75 = 11809u;
__riscv_vsetvl_e16mf4(1);
vint16mf4_t var_22 = __riscv_vle16_v_i16mf4(var_128, 1);
// 23919

vint16mf4_t var_23 = __riscv_vle16_v_i16mf4(var_127, 1);
// -24506

vuint8mf8_t var_25 = __riscv_vle8_v_u8mf8(var_125, 1);
// 7

vuint8mf8_t var_26 = __riscv_vle8_v_u8mf8(var_124, 1);
// 19

__riscv_vsetvl_e8m1(2);
vint8m1_t var_28 = __riscv_vle8_v_i8m1(var_123, 2);
// 127, -119

vint8m1_t var_29 = __riscv_vle8_v_i8m1(var_122, 2);
// -45, 89

__riscv_vsetvl_e32mf2(2);
vuint32mf2_t var_31 = __riscv_vle32_v_u32mf2(var_120, 2);
// 3102867433, 3759812467

vuint32mf2_t var_32 = __riscv_vle32_v_u32mf2(var_119, 2);
// 3244721859, 17611546

__riscv_vsetvl_e8mf2(2);
vint8mf2_t var_34 = __riscv_vle8_v_i8mf2(var_118, 2);
// -54, 20

vint8mf2_t var_35 = __riscv_vle8_v_i8mf2(var_117, 2);
// 0, 121

vuint16m1_t var_36 = __riscv_vle16_v_u16m1(var_116, 2);
// 48601, 39502

vuint16m1_t var_37 = __riscv_vle16_v_u16m1(var_115, 2);
// 55110, 60687

__riscv_vsetvl_e16mf4(1);
vint16mf4_t var_39 = __riscv_vle16_v_i16mf4(var_114, 1);
// -22328

vint16mf4_t var_40 = __riscv_vle16_v_i16mf4(var_113, 1);
// 28601

vfloat32mf2_t var_41 = __riscv_vle32_v_f32mf2(var_112, 1);
// -4.3892153800406675e-11

__riscv_vsetvl_e8mf8(2);
vint8mf8_t var_43 = __riscv_vle8_v_i8mf8(var_111, 2);
// 69, 42

vint8mf8_t var_44 = __riscv_vle8_v_i8mf8(var_110, 2);
// 89, 1

vuint8mf8_t var_45 = __riscv_vle8_v_u8mf8(var_109, 2);
// 78, 68

vint8mf8_t var_47 = __riscv_vle8_v_i8mf8(var_108, 2);
// -76, 47

vint8mf8_t var_48 = __riscv_vle8_v_i8mf8(var_107, 2);
// 12, 94

vuint64m1_t var_49 = __riscv_vle64_v_u64m1(var_106, 2);
// 18400157712069531911, 5717040025913777552

vuint64m1_t var_50 = __riscv_vle64_v_u64m1(var_105, 2);
// 13184553937407245441, 14207308214616682331

vuint64m1_t var_51 = __riscv_vle64_v_u64m1(var_104, 2);
// 2157312243695362217, 14090007358265115459

vuint8mf8_t var_54 = __riscv_vle8_v_u8mf8(var_102, 2);
// 45, 70

vuint8mf8_t var_55 = __riscv_vle8_v_u8mf8(var_101, 2);
// 183, 68

vuint8mf8_t var_56 = __riscv_vle8_v_u8mf8(var_100, 2);
// 95, 83

__riscv_vsetvl_e8m4(2);
vint8m4_t var_57 = __riscv_vle8_v_i8m4(var_99, 2);
// -112, -63

__riscv_vsetvl_e16mf4(1);
vuint16mf4_t var_60 = __riscv_vle16_v_u16mf4(var_97, 1);
// 27441

vuint8mf8_t var_62 = __riscv_vle8_v_u8mf8(var_96, 1);
// 86

vint32mf2_t var_64 = __riscv_vle32_v_i32mf2(var_94, 1);
// 1349881114

vint32mf2_t var_65 = __riscv_vle32_v_i32mf2(var_93, 1);
// -1370289438

__riscv_vsetvl_e16m1(2);
vint16m1_t var_67 = __riscv_vle16_v_i16m1(var_92, 2);
// -28549, -886

vuint8mf2_t var_68 = __riscv_vle8_v_u8mf2(var_91, 2);
// 24, 242

vuint8mf2_t var_69 = __riscv_vle8_v_u8mf2(var_90, 2);
// 96, 57

__riscv_vsetvl_e8mf8(1);
vuint8mf8_t var_70 = __riscv_vle8_v_u8mf8(var_89, 1);
// 109

vuint8mf8_t var_71 = __riscv_vle8_v_u8mf8(var_88, 1);
// 150

__riscv_vsetvl_e32mf2(2);
vfloat32mf2_t var_73 = __riscv_vle32_v_f32mf2(var_87, 2);
// 1.3251208240642717e-21, 2.5150643789828564e-09

vfloat32mf2_t var_74 = __riscv_vle32_v_f32mf2(var_86, 2);
// -1.0460544631490656e+22, 2.338682356976554e+23

__riscv_vsetvl_e8mf8(1);
vbool64_t var_21 = __riscv_vmadc_vv_u8mf8_b64(var_25, var_26, 1);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32mf2(2);
vbool64_t var_27 = __riscv_vmsbc_vv_u32mf2_b64(var_31, var_32, 2);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e16m1(2);
vbool16_t var_33 = __riscv_vmsltu_vv_u16m1_b16(var_36, var_37, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32mf2(1);
vbool64_t var_38 = __riscv_vmfge_vf_f32mf2_b64(var_41, var_77, 1);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e8mf8(2);
vbool64_t var_42 = __riscv_vmsne_vx_u8mf8_b64(var_45, var_76, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vbool64_t var_46 = __riscv_vmadc_vv_i8mf8_b64(var_47, var_48, 2);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vbool64_t var_53 = __riscv_vmsltu_vv_u8mf8_b64(var_55, var_56, 2);
// 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e8m4(2);
int8_t var_9 = __riscv_vmv_x_s_i8m4_i8(var_57);
// -112

__riscv_vsetvl_e16mf4(1);
vbool64_t var_58 = __riscv_vmseq_vx_u16mf4_b64(var_60, var_75, 1);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vbool64_t var_61 = __riscv_vmadc_vv_i32mf2_b64(var_64, var_65, 1);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e8mf2(2);
vbool16_t var_66 = __riscv_vmsltu_vv_u8mf2_b16(var_68, var_69, 2);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32mf2(2);
vbool64_t var_72 = __riscv_vmfeq_vv_f32mf2_b64(var_73, var_74, 2);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e8mf2(2);
vint8mf2_t var_16 = __riscv_vnmsac_vx_i8mf2_mu(var_33, var_34, var_79, var_35, 2);
// -54, -98

__riscv_vsetvl_e8mf8(2);
vint8mf8_t var_14 = __riscv_vsll_vx_i8mf8_mu(var_42, var_43, var_44, var_80, 2);
// 89, 1

__riscv_vsetvl_e8mf2(2);
vint8mf2_t var_5 = __riscv_vnsra_wx_i8mf2_mu(var_66, var_16, var_67, var_83, 2);
// 61, -98

__riscv_vsetvl_e8mf8(2);
vint8mf8_t var_11 = __riscv_vmv_s_x_i8mf8_tu(var_14, var_81, 2);
// 5, 1

vint8mf8_t var_13 = __riscv_vrem_vv_i8mf8_mu(var_46, var_14, var_14, var_14, 2);
// 89, 1

__riscv_vsetvl_e8mf2(2);
int8_t var_3 = __riscv_vmv_x_s_i8mf2_i8(var_5);
// 61

__riscv_vsetvl_e8mf8(1);
vint8mf8_t var_10 = __riscv_vsra_vv_i8mf8_mu(var_53, var_11, var_13, var_54, 1);
// 5, 1

vint8mf8_t var_1 = __riscv_vmax_vx_i8mf8_mu(var_72, var_10, var_10, var_9, 1);
// 5, 1

vint8mf8_t var_0 = __riscv_vssra_vx_i8mf8(var_1, var_85, 1);
// 5

vbool64_t var_2 = __riscv_vmsbc_vx_i8mf8_b64(var_0, var_3, 1);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vint8mf8_t var_7 = __riscv_vmadd_vv_i8mf8(var_0, var_10, var_10, 1);
// 30

vint8mf8_t var_8 = __riscv_vnmsac_vv_i8mf8_mu(var_58, var_0, var_10, var_10, 1);
// 5

vint16mf4_t var_15 = __riscv_vwadd_wv_i16mf4_mu(var_38, var_39, var_40, var_0, 1);
// 28606

vint8m1_t var_17 = __riscv_vredmax_vs_i8mf8_i8m1_tum(var_27, var_28, var_0, var_29, 1);
// 5

vuint8mf8_t var_4 = __riscv_vslidedown_vx_u8mf8_mu(var_2, var_70, var_71, var_84, 1);
// 150

vuint64m1_t var_12 = __riscv_vminu_vv_u64m1_mu(var_2, var_49, var_50, var_51, 1);
// 2157312243695362217, 5717040025913777552

vint8mf8_t var_19 = __riscv_vxor_vx_i8mf8(var_7, var_78, 1);
// -3

__riscv_vse8_v_i8mf8(var_98, var_8, 1);
vint16mf4_t var_18 = __riscv_vnmsub_vv_i16mf4_mu(var_21, var_15, var_22, var_23, 1);
// 28606

__riscv_vsetvl_e8m1(1);
__riscv_vse8_v_i8m1(var_121, var_17, 1);
__riscv_vsetvl_e8mf8(1);
vuint8mf8_t var_6 = __riscv_vslide1down_vx_u8mf8_mu(var_61, var_4, var_62, var_82, 1);
// 150

__riscv_vse64_v_u64m1(var_103, var_12, 1);
__riscv_vse8_v_i8mf8(var_129, var_19, 1);
if(!check(var_98, var_143, var_144)) {cerr << "check 142 fails" << endl; return_value = 1;}
__riscv_vse16_v_i16mf4(var_126, var_18, 1);
if(!check(var_121, var_137, var_138)) {cerr << "check 136 fails" << endl; return_value = 1;}
__riscv_vse8_v_u8mf8(var_95, var_6, 1);
if(!check(var_103, var_140, var_141)) {cerr << "check 139 fails" << endl; return_value = 1;}
if(!check(var_129, var_131, var_132)) {cerr << "check 130 fails" << endl; return_value = 1;}
if(!check(var_126, var_134, var_135)) {cerr << "check 133 fails" << endl; return_value = 1;}
if(!check(var_95, var_146, var_147)) {cerr << "check 145 fails" << endl; return_value = 1;}
if (return_value)
  __builtin_abort ();
return return_value;
}
