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

int main()
{
int return_value = 0;

size_t var_135 = 8u;
int64_t var_134 [] = {5319067641524761504};
size_t var_132 = 8u;
int64_t var_131 [] = {-2042112153301316246};
size_t var_129 = 2u;
uint8_t var_128 [] = {225u, 96u};
size_t var_126 = 1u;
int8_t var_125 [] = {61};
size_t var_123 = 8u;
int64_t var_122 [] = {1498010109281213184};
size_t var_120 = 8u;
uint32_t var_119 [] = {3077557042u, 4186139873u};
uint8_t var_117 [] = {223u, 9u};
uint8_t var_116 [] = {113u, 240u};
int16_t var_115 [] = {-24455, -11411};
int16_t var_114 [] = {-24094, -10755};
uint32_t var_113 [] = {2735010713u, 1592352930u};
uint32_t var_112 [] = {1541161051u, 1910154620u};
uint32_t var_111 [] = {3389314231u, 1014755114u};
int32_t var_110 [] = {-1621904547, -1040980082};
int32_t var_109 [] = {122711676, 407948494};
int64_t var_108 [] = {4536618263518479242};
int64_t var_107 [] = {1448075913413164717};
uint32_t var_106 [] = {3077557042u, 4186139873u};
uint32_t var_105 [] = {2817571044u, 2761811578u};
int32_t var_104 [] = {569342414, 172300542};
uint8_t var_103 [] = {206u, 239u};
int8_t var_102 [] = {52};
int8_t var_101 [] = {61};
int8_t var_100 [] = {21};
uint16_t var_99 [] = {33259u};
uint16_t var_98 [] = {37610u};
int64_t var_97 [] = {-2042112153301316246, -4719722743472540442};
int32_t var_96 [] = {-1613882636, 1783704109};
int64_t var_95 [] = {1806342126325465770, 2692019032660383081};
int64_t var_94 [] = {-5319067641524761504, -5232821194264970572};
uint16_t var_93 [] = {25960u, 64060u};
uint16_t var_92 [] = {46113u, 46688u};
uint8_t var_91 [] = {50u, 2u};
uint32_t var_90 [] = {2713597456u, 1668658475u};
int64_t var_89 [] = {627921155655533422};
int64_t var_88 [] = {5582011634372815520, 1817371833675277959};
int64_t var_87 [] = {-6317261412230281440, 21956431008910011};
int64_t var_86 [] = {1178141186383409279, 5961794512723147693};
int64_t var_85 [] = {227003041032385881, 4322780385860234349};
int64_t var_84 [] = {-5019114150361525821};
int32_t var_83 [] = {-1971716705};
int32_t var_82 [] = {931599149};
int16_t var_81 [] = {30933, 23703};
int16_t var_80 [] = {29162, -27337};
uint8_t var_79 [] = {68u, 91u};
uint8_t var_78 [] = {214u, 150u};
uint8_t var_77 [] = {242u, 207u};
size_t var_76 = 0u;
int32_t var_75 = 723490258;
uint32_t var_74 = 884830447u;
uint8_t var_73 = 144u;
int32_t var_72 = -1102902389;
int8_t var_71 = -121;
uint8_t var_70 = 79u;
__riscv_vsetvl_e8mf8(2);
vuint8mf8_t var_21 = __riscv_vle8_v_u8mf8(var_117, 2);
// 223, 9

vuint8mf8_t var_22 = __riscv_vle8_v_u8mf8(var_116, 2);
// 113, 240

vint16mf4_t var_23 = __riscv_vle16_v_i16mf4(var_115, 2);
// -24455, -11411

vint16mf4_t var_24 = __riscv_vle16_v_i16mf4(var_114, 2);
// -24094, -10755

vuint32mf2_t var_26 = __riscv_vle32_v_u32mf2(var_113, 2);
// 2735010713, 1592352930

vuint32mf2_t var_27 = __riscv_vle32_v_u32mf2(var_112, 2);
// 1541161051, 1910154620

vint32mf2_t var_29 = __riscv_vle32_v_i32mf2(var_110, 2);
// -1621904547, -1040980082

vint32mf2_t var_30 = __riscv_vle32_v_i32mf2(var_109, 2);
// 122711676, 407948494

__riscv_vsetvl_e64m1(1);
vint64m1_t var_31 = __riscv_vle64_v_i64m1(var_108, 1);
// 4536618263518479242

__riscv_vsetvl_e32mf2(2);
vuint32mf2_t var_34 = __riscv_vle32_v_u32mf2(var_106, 2);
// 3077557042, 4186139873

vuint32mf2_t var_35 = __riscv_vle32_v_u32mf2(var_105, 2);
// 2817571044, 2761811578

vint32mf2_t var_36 = __riscv_vle32_v_i32mf2(var_104, 2);
// 569342414, 172300542

__riscv_vsetvl_e8m8(2);
vuint8m8_t var_37 = __riscv_vle8_v_u8m8(var_103, 2);
// 206, 239

__riscv_vsetvl_e8mf8(1);
vint8mf8_t var_41 = __riscv_vle8_v_i8mf8(var_101, 1);
// 61

vint8mf8_t var_42 = __riscv_vle8_v_i8mf8(var_100, 1);
// 21

vuint16mf4_t var_43 = __riscv_vle16_v_u16mf4(var_99, 1);
// 33259

vuint16mf4_t var_44 = __riscv_vle16_v_u16mf4(var_98, 1);
// 37610

__riscv_vsetvl_e64m4(2);
vint64m4_t var_45 = __riscv_vle64_v_i64m4(var_97, 2);
// -2042112153301316246, -4719722743472540442

vint32m2_t var_46 = __riscv_vle32_v_i32m2(var_96, 2);
// -1613882636, 1783704109

__riscv_vsetvl_e64m1(2);
vint64m1_t var_48 = __riscv_vle64_v_i64m1(var_95, 2);
// 1806342126325465770, 2692019032660383081

vint64m1_t var_49 = __riscv_vle64_v_i64m1(var_94, 2);
// -5319067641524761504, -5232821194264970572

vuint16mf4_t var_50 = __riscv_vle16_v_u16mf4(var_93, 2);
// 25960, 64060

vuint16mf4_t var_51 = __riscv_vle16_v_u16mf4(var_92, 2);
// 46113, 46688

vuint32mf2_t var_53 = __riscv_vle32_v_u32mf2(var_90, 2);
// 2713597456, 1668658475

__riscv_vsetvl_e64m4(2);
vint64m4_t var_55 = __riscv_vle64_v_i64m4(var_88, 2);
// 5582011634372815520, 1817371833675277959

vint64m4_t var_56 = __riscv_vle64_v_i64m4(var_87, 2);
// -6317261412230281440, 21956431008910011

vint64m4_t var_57 = __riscv_vle64_v_i64m4(var_86, 2);
// 1178141186383409279, 5961794512723147693

vint64m4_t var_58 = __riscv_vle64_v_i64m4(var_85, 2);
// 227003041032385881, 4322780385860234349

__riscv_vsetvl_e32mf2(1);
vint32mf2_t var_61 = __riscv_vle32_v_i32mf2(var_83, 1);
// -1971716705

vint32mf2_t var_62 = __riscv_vle32_v_i32mf2(var_82, 1);
// 931599149

__riscv_vsetvl_e16mf4(2);
vint16mf4_t var_65 = __riscv_vle16_v_i16mf4(var_81, 2);
// 30933, 23703

vint16mf4_t var_66 = __riscv_vle16_v_i16mf4(var_80, 2);
// 29162, -27337

vuint8mf8_t var_67 = __riscv_vle8_v_u8mf8(var_79, 2);
// 68, 91

vuint8mf8_t var_68 = __riscv_vle8_v_u8mf8(var_78, 2);
// 214, 150

vuint8mf8_t var_69 = __riscv_vle8_v_u8mf8(var_77, 2);
// 242, 207

vbool64_t var_20 = __riscv_vmsbc_vv_i16mf4_b64(var_23, var_24, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vbool64_t var_25 = __riscv_vmsbc_vv_i32mf2_b64(var_29, var_30, 2);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vbool64_t var_33 = __riscv_vmadc_vx_i32mf2_b64(var_36, var_72, 2);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e8m8(2);
uint8_t var_15 = __riscv_vmv_x_s_u8m8_u8(var_37);
// 206

__riscv_vsetvl_e8mf8(1);
vbool64_t var_39 = __riscv_vmsge_vx_i8mf8_b64(var_42, var_71, 1);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vbool64_t var_38 = __riscv_vmsne_vv_u16mf4_b64(var_43, var_44, 1);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e64m4(2);
//int64_t var_13;
//__riscv_vse64_v_i64m4 (&var_13, var_45, 1);
int64_t var_13 = __riscv_vmv_x_s_i64m4_i64(var_45);
// -2042112153301316246

vbool16_t var_12 = __riscv_vmsgt_vx_i32m2_b16(var_46, var_75, 2);
// 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e16mf4(2);
vbool64_t var_47 = __riscv_vmsne_vv_u16mf4_b64(var_50, var_51, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32mf2(1);
vbool64_t var_59 = __riscv_vmsne_vv_i32mf2_b64(var_61, var_62, 1);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e8mf8(2);
vbool64_t var_64 = __riscv_vmsne_vv_u8mf8_b64(var_67, var_68, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vbool64_t var_63 = __riscv_vmseq_vx_u8mf8_b64(var_69, var_70, 2);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vuint8mf8_t var_19 = __riscv_vsub_vx_u8mf8_tumu(var_20, var_21, var_22, var_73, 2);
// 225, 96

vuint32mf2_t var_16 = __riscv_vssubu_vx_u32mf2_tumu(var_33, var_34, var_35, var_74, 2);
// 3077557042, 4186139873

__riscv_vsetvl_e64m4(2);
vint64m4_t var_5 = __riscv_vor_vv_i64m4_tumu(var_12, var_56, var_57, var_58, 2);
// -6317261412230281440, 8934470820093100013

__riscv_vsetvl_e64m1(2);
vint64m1_t var_11 = __riscv_vneg_v_i64m1_tumu(var_47, var_48, var_49, 2);
// 5319067641524761504, 5232821194264970572

vbool64_t var_2 = __riscv_vmsne_vv_i16mf4_b64_mu(var_63, var_64, var_65, var_66, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e64m4(2);
int64_t var_4 = __riscv_vmv_x_s_i64m4_i64(var_5);
//int64_t var_4;
//__riscv_vse64_v_i64m4 (&var_4, var_5, 1);
// -6317261412230281440

__riscv_vsetvl_e64m1(2);
vint64m1_t var_1 = __riscv_vmacc_vv_i64m1_tumu(var_2, var_11, var_11, var_11, 2);
// -9179520755515992160, 6485801283920106460

//vint64m1_t tmp_0 = __riscv_vmv_v_x_i64m1 (var_4, 2);
vint64m1_t var_0 = __riscv_vrsub_vx_i64m1_tumu(var_2, var_1, var_1, var_4, 2);
// 2862259343285710720, 5643681377559163716

__riscv_vsetvl_e64m1(1);
vint64m1_t var_3 = __riscv_vslidedown_vx_i64m1_tumu(var_59, var_0, var_11, var_76, 1);
// 5319067641524761504

__riscv_vsetvl_e64m4(2);
vint64m1_t var_6 = __riscv_vredsum_vs_i64m4_i64m1_tu(var_0, var_55, var_11, 2);
// -5728292964136696633

__riscv_vsetvl_e64m1(2);
vbool64_t var_8 = __riscv_vmadc_vv_i64m1_b64(var_0, var_11, 2);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e64m1(1);
vint64m1_t var_17 = __riscv_vmul_vv_i64m1(var_0, var_31, 1);
// 1498010109281213184

__riscv_vse64_v_i64m1(var_84, var_3, 1);
vint64m1_t var_7 = __riscv_vmv_s_x_i64m1_tu(var_6, var_13, 1);
// -2042112153301316246, 5643681377559163716

__riscv_vsetvl_e32mf2(2);
vuint32mf2_t var_9 = __riscv_vdivu_vv_u32mf2_tumu(var_8, var_16, var_16, var_53, 2);
// 3077557042, 4186139873

vuint8mf8_t var_10 = __riscv_vmerge_vxm_u8mf8(var_19, var_15, var_8, 2);
// 225, 96

__riscv_vsetvl_e64m1(1);
__riscv_vse64_v_i64m1(var_107, var_17, 1);
if(!check(var_84, var_134, var_135)) {cerr << "check 133 fails" << endl; return_value = 1;}
__riscv_vse64_v_i64m1(var_89, var_7, 1);
vbool64_t var_14 = __riscv_vmsgeu_vv_u32mf2_b64_mu(var_38, var_39, var_9, var_16, 1);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32mf2(2);
vuint32mf2_t var_18 = __riscv_vmacc_vv_u32mf2_tumu(var_25, var_9, var_26, var_27, 2);
// 3077557042, 4186139873

__riscv_vse8_v_u8mf8(var_91, var_10, 2);
if(!check(var_107, var_122, var_123)) {cerr << "check 121 fails" << endl; return_value = 1;}
if(!check(var_89, var_131, var_132)) {cerr << "check 130 fails" << endl; return_value = 1;}
__riscv_vsetvl_e8mf8(1);
__riscv_vse8_v_i8mf8_m(var_14, var_102, var_41, 1);
__riscv_vsetvl_e32mf2(2);
__riscv_vse32_v_u32mf2(var_111, var_18, 2);
if(!check(var_91, var_128, var_129)) {cerr << "check 127 fails" << endl; return_value = 1;}
if(!check(var_102, var_125, var_126)) {cerr << "check 124 fails" << endl; return_value = 1;}
if(!check(var_111, var_119, var_120)) {cerr << "check 118 fails" << endl; return_value = 1;}
if (return_value)
__builtin_abort ();
}
