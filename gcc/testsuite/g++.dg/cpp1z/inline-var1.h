inline int var1 = 4;
static inline int var7 = 9;
namespace N
{
  int inline var2;
  inline const int var6 = 8;
  static inline double var8 = 2.0;
  extern inline char var10;
}
struct S
{
  static constexpr int var3 = 5;
  static inline int var4 = 6;
  static constexpr int var5 = 7;
  static inline double var9 = 3.0;
  static constexpr inline int var11 = 11;
};
const int S::var3;
const int S::var3;
extern int foo (int);
extern int bar (int);
struct T { T () { t = foo (3); } T (int x) { t = foo (x); } int t; };
inline int var12 = foo (0);
int inline var13 = foo (1);
struct U
{
  static inline int var14 = foo (2);
  static inline T var15;
  static inline T var16 = 4;
  static int inline var17 = foo (5);
  static constexpr double var18 = 4.0;
};
template <typename T>
struct Y
{
  static constexpr T var24 = 6;
  static inline T var25 = 7;
  static inline int var26 = 8;
  static constexpr T var28 = 10;
};
template <typename T>
const T Y<T>::var24;
template <typename T>
const T Y<T>::var24;
template <typename T>
inline T var27 = 9;
