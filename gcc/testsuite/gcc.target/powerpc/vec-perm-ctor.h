#include "altivec.h"

typedef vector unsigned long long v2du;
typedef vector signed long long v2di;
typedef vector unsigned int v4su;
typedef vector signed int v4si;
typedef vector unsigned short v8hu;
typedef vector signed short v8hi;
typedef vector unsigned char v16qu;
typedef vector signed char v16qi;
typedef vector double v2df;
typedef vector float v4sf;

typedef unsigned long long du;
typedef signed long long di;
typedef unsigned int su;
typedef signed int si;
typedef unsigned short hu;
typedef signed short hi;
typedef unsigned char qu;
typedef signed char qi;
typedef double df;
typedef float sf;

/* To test whether we can optimize vector permutation away when
   the two inputs are same type CTOR or one input is CTOR and the
   other is CST.  */

/* CTOR + CTOR part (only same type supported).  */

/* Test both operands are same type CTOR (type unsigned long long).  */
__attribute__ ((noipa)) v2du
test_ctor_ctor_same_du (du a, du b)
{
  v2du v1 = {a, 0};
  v2du v2 = {b, 0};
  v16qu vc = {0, 1, 2, 3, 4, 5, 6, 7, 16, 17, 18, 19, 20, 21, 22, 23};
  v2du vres = (v2du) vec_perm ((v16qu) v1, (v16qu) v2, vc);
  return vres;
}

/* Test both operands are same type CTOR (type double).  */
__attribute__ ((noipa)) v2df
test_ctor_ctor_same_df (df a, df b)
{
  v2df v1 = {0.0, a};
  v2df v2 = {0.0, b};
  v16qu vc = {8, 9, 10, 11, 12, 13, 14, 15, 24, 25, 26, 27, 28, 29, 30, 31};
  v2df vres = (v2df) vec_perm ((v16qu) v1, (v16qu) v2, vc);
  return vres;
}

/* Test both operands are same type CTOR (type signed int).  */
__attribute__ ((noipa)) v4si
test_ctor_ctor_same_si (si a, si b, si c, si d)
{
  v4si v1 = {0, a, 0, c};
  v4si v2 = {0, b, 0, d};
  v16qu vc = {4, 5, 6, 7, 20, 21, 22, 23, 12, 13, 14, 15, 28, 29, 30, 31};
  v4si vres = (v4si) vec_perm ((v16qu) v1, (v16qu) v2, vc);
  return vres;
}

/* Test both operands are same type CTOR (type float).  */
__attribute__ ((noipa)) v4sf
test_ctor_ctor_same_sf (sf a, sf b, sf c, sf d)
{
  v4sf v1 = {c, 0.0f, d, 0.0f};
  v4sf v2 = {a, 0.0f, b, 0.0f};
  v16qu vc = {16, 17, 18, 19, 24, 25, 26, 27, 0, 1, 2, 3, 8, 9, 10, 11};
  v4sf vres = (v4sf) vec_perm ((v16qu) v1, (v16qu) v2, vc);
  return vres;
}

/* Test both operands are same type CTOR (type unsigned short).  */
__attribute__ ((noipa)) v8hu
test_ctor_ctor_same_hu (hu a, hu b, hu c, hu d, hu e, hu f, hu g, hu h)
{
  v8hu v1 = {0, a, 0, b, 0, c, 0, d};
  v8hu v2 = {0, e, 0, f, 0, g, 0, h};
  v16qu vc = {2, 3, 6, 7, 10, 11, 14, 15, 18, 19, 22, 23, 26, 27, 30, 31};
  v8hu vres = (v8hu) vec_perm ((v16qu) v1, (v16qu) v2, vc);
  return vres;
}

/* Test both operands are same type CTOR (type signed char).  */
__attribute__ ((noipa)) v16qi
test_ctor_ctor_same_qi (qi a, qi b, qi c, qi d, qi e, qi f, qi g, qi h)
{
  v16qi v1 = {0, a, 0, b, 0, c, 0, d, 0, a, 0, b, 0, c, 0, d};
  v16qi v2 = {0, e, 0, f, 0, g, 0, h, 0, e, 0, f, 0, g, 0, h};
  v16qu vc = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31};
  v16qi vres = (v16qi) vec_perm ((v16qu) v1, (v16qu) v2, vc);
  return vres;
}

/* CTOR + CST part (same type).  */

__attribute__ ((noipa)) v2du
test_ctor_cst_same_du (du a, du b)
{
  v2du v1 = {a, b};
  v2du v2 = {100, 200};
  v16qu vc = {0, 1, 2, 3, 4, 5, 6, 7, 16, 17, 18, 19, 20, 21, 22, 23};
  v2du vres = (v2du) vec_perm ((v16qu) v1, (v16qu) v2, vc);
  return vres;
}

__attribute__ ((noipa)) v4sf
test_ctor_cst_same_sf (sf a, sf b)
{
  v4sf v1 = {0.0f, a, 0.0f, b};
  v4sf v2 = {1.0f, 2.0f, 3.0f, 4.0f};
  v16qu vc = {4, 5, 6, 7, 20, 21, 22, 23, 12, 13, 14, 15, 28, 29, 30, 31};
  v4sf vres = (v4sf) vec_perm ((v16qu) v1, (v16qu) v2, vc);
  return vres;
}

/* CST + CTOR part (same type).  */

__attribute__ ((noipa)) v2df
test_ctor_cst_same_df (df a, df b)
{
  v2df v1 = {a, b};
  v2df v2 = {100.0, 200.0};
  v16qu vc = {8, 9, 10, 11, 12, 13, 14, 15, 24, 25, 26, 27, 28, 29, 30, 31};
  v2df vres = (v2df) vec_perm ((v16qu) v1, (v16qu) v2, vc);
  return vres;
}

__attribute__ ((noipa)) v4si
test_cst_ctor_same_si (si a, si b)
{
  v4si v1 = {a, 0, b, 0};
  v4si v2 = {1, 2, 3, 4};
  v16qu vc = {16, 17, 18, 19, 24, 25, 26, 27, 0, 1, 2, 3, 8, 9, 10, 11};
  v4si vres = (v4si) vec_perm ((v16qu) v1, (v16qu) v2, vc);
  return vres;
}

/* CTOR + CST part (different types).  */

__attribute__ ((noipa)) v2di
test_ctor_cst_diff_di_si (di a, di b)
{
  v2di v1 = {a, b};
  v4si v2 = {3, 0, 4, 0};
  v16qu vc = {0, 1, 2, 3, 4, 5, 6, 7, 16, 17, 18, 19, 20, 21, 22, 23};
  v2di vres = (v2di) vec_perm ((v16qu) v1, (v16qu) v2, vc);
  return vres;
}

/* CST + CTOR part (different types).  */

__attribute__ ((noipa)) v2du
test_cst_ctor_diff_sf_du (du a, du b)
{
  v4sf v1 = {1.0f, 2.0f, 3.0f, 4.0f};
  v2du v2 = {a, b};
  v16qu vc = {0, 1, 2, 3, 4, 5, 6, 7, 16, 17, 18, 19, 20, 21, 22, 23};
  v2du vres = (v2du) vec_perm ((v16qu) v1, (v16qu) v2, vc);
  return vres;
}
