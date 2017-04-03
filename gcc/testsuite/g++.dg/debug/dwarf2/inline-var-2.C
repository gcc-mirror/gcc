// { dg-do compile }
// { dg-options "-O -std=c++1z -gdwarf-5 -dA -gno-strict-dwarf" }
// { dg-require-weak "" }
// { dg-final { scan-assembler-not "DW_TAG_member" { xfail *-*-aix* } } }

inline int a;
struct S
{
  static inline double b = 4.0;
  static constexpr int c = 2;
  static constexpr inline char d = 3;
  static const int j = 7;
  static int k;
  static double l;
} s;
const int S::j;
int S::k = 8;
template <int N>
inline int e = N;
int &f = e<2>;
template <int N>
struct T
{
  static inline double g = 4.0;
  static constexpr int h = 2;
  static inline constexpr char i = 3;
  static const int m = 8;
  static int n;
  static double o;
};
T<5> t;
template <>
const int T<5>::m;
template <>
int T<5>::n = 9;
