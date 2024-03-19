/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target rv64 } */
/* { dg-require-effective-target riscv_v } */

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

size_t var_149 = 16u;
uint64_t var_148 [] = {13578039560782071336u, 1484621602351210644u};
size_t var_146 = 8u;
uint64_t var_145 [] = {16839166774797421612u};
size_t var_143 = 16u;
uint64_t var_142 [] = {14866568020778459593u, 6170176812097337211u};
size_t var_140 = 16u;
int32_t var_139 [] = {86596679, 887344792, 624253109, 1050186991};
size_t var_137 = 16u;
int32_t var_136 [] = {-1167308735, 1838210492, -189423925, 1242164643};
size_t var_134 = 16u;
int32_t var_133 [] = {1059536784, -1624607338, -1986213221, -1866499287};
size_t var_131 = 16u;
int32_t var_130 [] = {-1290998524, -395147148, 1204902894, -1979270528};
size_t var_128 = 16u;
int32_t var_127 [] = {1505028399, -1317551444, -283215388, 1313442090};
size_t var_125 = 16u;
int32_t var_124 [] = {669327590, -1638382142, -1415276961, 770276165};
size_t var_122 = 16u;
int32_t var_121 [] = {1961139923, 470559939, -1893737022, -808529908};
size_t var_119 = 16u;
int32_t var_118 [] = {861564409, 1961139923, 470559939, 683032407};
uint64_t var_116 [] = {14884245833012991004u, 17744737836532097112u};
int32_t var_115 [] = {861564409, -280631346, -1997213588, 683032407};
uint64_t var_113 [] = {8u, 4u};
int32_t var_111 [] = {-550820466, -659945020, -1893737022, -808529908};
uint64_t var_110 [] = {4u, 0u};
int32_t var_108 [] = {1960574163, -1638382142, -2055339996, 770276165};
uint64_t var_107 [] = {8u, 0u};
uint64_t var_105 [] = {15667108066432247254u};
int32_t var_104 [] = {1505028399, 11159289, -283215388, 1313442090};
uint64_t var_102 [] = {4u};
int32_t var_100 [] = {-1215052017, -395147148, 2746584, -1979270528};
uint64_t var_99 [] = {8u, 0u};
int32_t var_97 [] = {-1605026914, -1624607338, -1986213221, -1866499287};
uint64_t var_96 [] = {0u};
int32_t var_94 [] = {624695647, 1838210492, -189423925, 1242164643};
uint64_t var_93 [] = {0u};
uint64_t var_91 [] = {2254556060317310634u, 15112742239148579977u};
int32_t var_90 [] = {86596679, 887344792, 864165312, -748092779};
uint64_t var_88 [] = {12u, 8u};
uint64_t var_86 [] = {13226533873259168368u, 3099208707140262961u};
uint32_t var_85 [] = {4169354542u, 2687237004u};
int16_t var_84 [] = {-29404, 19521};
uint16_t var_83 [] = {51041u, 706u};
uint8_t var_82 [] = {72u, 130u};
uint8_t var_81 [] = {183u, 12u};
int64_t var_80 [] = {-1976754641945385291, -6857509168378727892};
uint64_t var_79 [] = {14866568020778459593u, 6170176812097337211u};
int32_t var_78 [] = {1050186991, 624253109};
uint64_t var_77 [] = {13043091477560443126u, 4018444280476919261u};
uint64_t var_76 [] = {1619119734530915404u, 1567408130103000042u};
int64_t var_75 [] = {-4589669510099122819, 2511201338146659042};
int64_t var_74 [] = {5192924483735233374, -5128303964436690562};
uint64_t var_73 [] = {10770195364104292946u, 11113326431322530895u};
int32_t var_72 [] = {498936736, -1494726800};
int32_t var_71 [] = {-323146212, -139718171};
int16_t var_70 [] = {18537, -8550};
int16_t var_69 [] = {-4141, 2189};
int32_t var_68 [] = {-1167308735, 1432744780};
uint64_t var_67 [] = {1032786338249332386u, 13739438568806714233u};
int32_t var_66 [] = {1059536784};
int32_t var_65 [] = {1204902894, -1290998524};
int32_t var_64 [] = {-1317551444, -1026693561};
int32_t var_63 [] = {-1415276961, 669327590};
int32_t var_62 [] = {-1269273947, -597221977};
int32_t var_61 [] = {470559939, 1961139923};
uint32_t var_60 [] = {2453925045u, 719126702u};
uint32_t var_59 [] = {2144310743u, 424531284u};
uint16_t var_58 = 47940u;
uint16_t var_57 = 17196u;
uint16_t var_56 = 3990u;
int32_t var_55 = 5337296;
uint32_t var_54 = 1248327624u;
int16_t var_53 = -7374;
int64_t var_52 = -970238298900334018;
__riscv_vsetvl_e64m1(2);
vuint64m1_t var_112 = __riscv_vle64_v_u64m1(var_113, 2);
// 8, 4

vuint64m1_t var_109 = __riscv_vle64_v_u64m1(var_110, 2);
// 4, 0

vuint64m1_t var_106 = __riscv_vle64_v_u64m1(var_107, 2);
// 8, 0

__riscv_vsetvl_e64m1(1);
vuint64m1_t var_101 = __riscv_vle64_v_u64m1(var_102, 1);
// 4

__riscv_vsetvl_e64m1(2);
vuint64m1_t var_98 = __riscv_vle64_v_u64m1(var_99, 2);
// 8, 0

__riscv_vsetvl_e64m1(1);
vuint64m1_t var_95 = __riscv_vle64_v_u64m1(var_96, 1);
// 0

vuint64m1_t var_92 = __riscv_vle64_v_u64m1(var_93, 1);
// 0

__riscv_vsetvl_e64m1(2);
vuint64m1_t var_87 = __riscv_vle64_v_u64m1(var_88, 2);
// 12, 8

__riscv_vsetvl_e64m2(2);
vuint64m2_t var_21 = __riscv_vle64_v_u64m2(var_86, 2);
// 13226533873259168368, 3099208707140262961

vuint32m1_t var_22 = __riscv_vle32_v_u32m1(var_85, 2);
// 4169354542, 2687237004

vint16mf2_t var_23 = __riscv_vle16_v_i16mf2(var_84, 2);
// -29404, 19521

__riscv_vsetvl_e16m2(2);
vuint16m2_t var_25 = __riscv_vle16_v_u16m2(var_83, 2);
// 51041, 706

vuint8m1_t var_26 = __riscv_vle8_v_u8m1(var_82, 2);
// 72, 130

vuint8m1_t var_27 = __riscv_vle8_v_u8m1(var_81, 2);
// 183, 12

vint64m8_t var_28 = __riscv_vle64_v_i64m8(var_80, 2);
// -1976754641945385291, -6857509168378727892

__riscv_vsetvl_e64m1(2);
vuint64m1_t var_29 = __riscv_vle64_v_u64m1(var_79, 2);
// 14866568020778459593, 6170176812097337211

vint32mf2_t var_30 = __riscv_vle32_v_i32mf2(var_78, 2);
// 1050186991, 624253109

vuint64m1_t var_32 = __riscv_vle64_v_u64m1(var_77, 2);
// 13043091477560443126, 4018444280476919261

vuint64m1_t var_33 = __riscv_vle64_v_u64m1(var_76, 2);
// 1619119734530915404, 1567408130103000042

__riscv_vsetvl_e64m2(2);
vint64m2_t var_34 = __riscv_vle64_v_i64m2(var_75, 2);
// -4589669510099122819, 2511201338146659042

vint64m2_t var_35 = __riscv_vle64_v_i64m2(var_74, 2);
// 5192924483735233374, -5128303964436690562

__riscv_vsetvl_e64m4(2);
vuint64m4_t var_36 = __riscv_vle64_v_u64m4(var_73, 2);
// 10770195364104292946, 11113326431322530895

__riscv_vsetvl_e32m4(2);
vint32m4_t var_37 = __riscv_vle32_v_i32m4(var_72, 2);
// 498936736, -1494726800

vint32m4_t var_38 = __riscv_vle32_v_i32m4(var_71, 2);
// -323146212, -139718171

vint16m2_t var_39 = __riscv_vle16_v_i16m2(var_70, 2);
// 18537, -8550

vint16m2_t var_40 = __riscv_vle16_v_i16m2(var_69, 2);
// -4141, 2189

__riscv_vsetvl_e32mf2(2);
vint32mf2_t var_41 = __riscv_vle32_v_i32mf2(var_68, 2);
// -1167308735, 1432744780

vuint64m1_t var_42 = __riscv_vle64_v_u64m1(var_67, 2);
// 1032786338249332386, 13739438568806714233

__riscv_vsetvl_e32mf2(1);
vint32mf2_t var_43 = __riscv_vle32_v_i32mf2(var_66, 1);
// 1059536784

__riscv_vsetvl_e32mf2(2);
vint32mf2_t var_44 = __riscv_vle32_v_i32mf2(var_65, 2);
// 1204902894, -1290998524

vint32mf2_t var_45 = __riscv_vle32_v_i32mf2(var_64, 2);
// -1317551444, -1026693561

vint32mf2_t var_46 = __riscv_vle32_v_i32mf2(var_63, 2);
// -1415276961, 669327590

vint32mf2_t var_48 = __riscv_vle32_v_i32mf2(var_62, 2);
// -1269273947, -597221977

vint32mf2_t var_49 = __riscv_vle32_v_i32mf2(var_61, 2);
// 470559939, 1961139923

vuint32mf2_t var_50 = __riscv_vle32_v_u32mf2(var_60, 2);
// 2453925045, 719126702

vuint32mf2_t var_51 = __riscv_vle32_v_u32mf2(var_59, 2);
// 2144310743, 424531284

__riscv_vsetvl_e16mf2(2);
vbool32_t var_20 = __riscv_vmslt_vx_i16mf2_b32(var_23, var_53, 2);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e64m8(2);
vbool8_t var_24 = __riscv_vmslt_vx_i64m8_b8(var_28, var_52, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e64m1(2);
__riscv_vse64_v_u64m1(var_91, var_29, 2);
__riscv_vsuxei64_v_i32mf2(var_90, var_87, var_30, 2);
__riscv_vsetvl_e64m2(2);
vbool32_t var_31 = __riscv_vmadc_vv_i64m2_b32(var_34, var_35, 2);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e64m4(2);
uint64_t var_15 = __riscv_vmv_x_s_u64m4_u64(var_36);
// 10770195364104292946

__riscv_vsetvl_e32m4(2);
vbool8_t var_14 = __riscv_vmsgt_vx_i32m4_b8(var_37, var_55, 2);
// 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vint32m4_t var_11 = __riscv_vwmaccus_vx_i32m4(var_38, var_58, var_39, 2);
// 565517568, -549605171

__riscv_vsetvl_e32mf2(1);
__riscv_vsuxei64_v_i32mf2(var_94, var_92, var_41, 1);
__riscv_vsuxei64_v_i32mf2(var_97, var_95, var_43, 1);
__riscv_vsetvl_e32mf2(2);
__riscv_vsuxei64_v_i32mf2(var_100, var_98, var_44, 2);
__riscv_vsetvl_e32mf2(1);
__riscv_vsuxei64_v_i32mf2(var_104, var_101, var_45, 1);
__riscv_vsetvl_e32mf2(2);
__riscv_vsuxei64_v_i32mf2(var_108, var_106, var_46, 2);
vbool64_t var_47 = __riscv_vmsgtu_vv_u32mf2_b64(var_50, var_51, 2);
// 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e32m1(2);
vuint64m2_t var_19 = __riscv_vwmaccu_vx_u64m2_mu(var_20, var_21, var_54, var_22, 2);
// 18431254322287636576, 3099208707140262961

__riscv_vsetvl_e8m1(2);
vuint16m2_t var_18 = __riscv_vwmulu_vv_u16m2_mu(var_24, var_25, var_26, var_27, 2);
// 13176, 1560

if(!check(var_91, var_142, var_143)) {cerr << "check 141 fails" << endl; return_value = 1;}
if(!check(var_90, var_139, var_140)) {cerr << "check 138 fails" << endl; return_value = 1;}
__riscv_vsetvl_e64m1(2);
vuint64m1_t var_7 = __riscv_vasubu_vx_u64m1(var_42, var_15, 0, 2);
// 13578039560782071336, 1484621602351210644

if(!check(var_94, var_136, var_137)) {cerr << "check 135 fails" << endl; return_value = 1;}
if(!check(var_97, var_133, var_134)) {cerr << "check 132 fails" << endl; return_value = 1;}
if(!check(var_100, var_130, var_131)) {cerr << "check 129 fails" << endl; return_value = 1;}
if(!check(var_104, var_127, var_128)) {cerr << "check 126 fails" << endl; return_value = 1;}
if(!check(var_108, var_124, var_125)) {cerr << "check 123 fails" << endl; return_value = 1;}
__riscv_vsetvl_e64m2(2);
vuint64m1_t var_16 = __riscv_vredxor_vs_u64m2_u64m1_tum(var_31, var_32, var_19, var_33, 2);
// 16839166774797421612

__riscv_vsetvl_e16m2(2);
vuint16m2_t var_13 = __riscv_vslide1down_vx_u16m2(var_18, var_56, 2);
// 1560, 3990

__riscv_vsetvl_e64m1(2);
__riscv_vse64_v_u64m1(var_116, var_7, 2);
__riscv_vsetvl_e64m1(1);
__riscv_vse64_v_u64m1(var_105, var_16, 1);
__riscv_vsetvl_e16m2(2);
vuint16m2_t var_12 = __riscv_vdivu_vx_u16m2(var_13, var_57, 2);
// 0, 0

if(!check(var_116, var_148, var_149)) {cerr << "check 147 fails" << endl; return_value = 1;}
if(!check(var_105, var_145, var_146)) {cerr << "check 144 fails" << endl; return_value = 1;}
vint32m4_t var_10 = __riscv_vwmulsu_vv_i32m4_mu(var_14, var_11, var_40, var_12, 2);
// 0, -549605171

int32_t var_9 = __riscv_vmv_x_s_i32m4_i32(var_10);
// 0

__riscv_vsetvl_e32mf2(2);
vint32mf2_t var_2 = __riscv_vsadd_vx_i32mf2_mu(var_47, var_48, var_49, var_9, 2);
// 470559939, 1961139923

__riscv_vsuxei64_v_i32mf2(var_115, var_112, var_2, 2);
__riscv_vsuxei64_v_i32mf2(var_111, var_109, var_2, 2);
if(!check(var_115, var_118, var_119)) {cerr << "check 117 fails" << endl; return_value = 1;}
if(!check(var_111, var_121, var_122)) {cerr << "check 120 fails" << endl; return_value = 1;}
if (return_value)
  __builtin_abort ();
return return_value;
}
