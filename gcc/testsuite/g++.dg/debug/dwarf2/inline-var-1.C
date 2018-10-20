// { dg-do compile { target c++17 } }
// { dg-options "-O -g -dA -gno-strict-dwarf" }
// { dg-require-weak "" }
// { dg-final { scan-assembler-times "0x3\[^\n\r]* DW_AT_inline" 6 { xfail *-*-aix* } } }
// { dg-final { scan-assembler-times "0x1\[^\n\r]* DW_AT_inline" 2 { xfail *-*-aix* } } }
// { dg-final { scan-assembler-times " DW_AT_declaration" 6 { xfail *-*-aix* } } }
// { dg-final { scan-assembler-times " DW_AT_specification" 6 { xfail *-*-aix* } } }
// { dg-final { scan-assembler-times " DW_AT_\[^\n\r]*linkage_name" 7 { xfail *-*-aix* } } }

inline int a;
struct S
{
  static inline double b = 4.0;
  static constexpr int c = 2;
  static constexpr inline char d = 3;
} s;
template <int N>
inline int e = N;
int &f = e<2>;
template <int N>
struct T
{
  static inline double g = 4.0;
  static constexpr int h = 2;
  static inline constexpr char i = 3;
};
T<5> t;
