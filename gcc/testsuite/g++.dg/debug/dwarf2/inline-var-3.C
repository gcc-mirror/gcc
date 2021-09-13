// DWARF5 variant of inline-var-1.C
// { dg-do compile { target c++17 } }
// { dg-options "-O -gdwarf-5 -dA -gno-strict-dwarf -fno-eliminate-unused-debug-symbols" }
// { dg-require-weak "" }
// { dg-skip-if "AIX DWARF5" { powerpc-ibm-aix* } }
// { dg-final { scan-assembler-times " DW_AT_inline \\(0x3\\)" 2 } }
// { dg-final { scan-assembler-times "0x3\[^\n\r]* DW_AT_inline" 4 } }
// { dg-final { scan-assembler-times "0x1\[^\n\r]* DW_AT_inline" 2 } }
// { dg-final { scan-assembler-times " DW_AT_declaration" 6 } }
// { dg-final { scan-assembler-times " DW_AT_specification" 6 } }
// { dg-final { scan-assembler-times " DW_AT_\[^\n\r]*linkage_name" 7 } }

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
