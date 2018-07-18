/* { dg-do run } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -fmath-errno -fdump-tree-cdce-details -lm" } */
/* { dg-additional-options "-DLARGE_LONG_DOUBLE" { target large_long_double } } */
/* { dg-additional-options "-DGNU_EXTENSION" { target pow10 } } */
/* { dg-add-options ieee } */
/* { dg-final { scan-tree-dump  "cdce3.C:91: .* function call is shrink-wrapped into error conditions\." "cdce" { target pow10 } } } */
/* { dg-final { scan-tree-dump  "cdce3.C:92: .* function call is shrink-wrapped into error conditions\." "cdce" { target pow10 } } } */
/* { dg-final { scan-tree-dump  "cdce3.C:94: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:95: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:96: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:97: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:98: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:99: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:100: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:101: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:102: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:103: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:104: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:105: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:106: .* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump  "cdce3.C:107: .* function call is shrink-wrapped into error conditions\." "cdce" } } */

#include <stdlib.h>
#include <math.h>
#ifdef DEBUG
#include <stdio.h>
#endif
#include <errno.h>
typedef void (*FP) (int xp);
#define NI __attribute__((noinline))

#if defined(LARGE_LONG_DOUBLE)
typedef long double ldouble; 
ldouble result;

#define DEF_MATH_FUNC(prefix, name) NI void prefix##name##f (int x) \
{ \
  float yy = name##f ((float) x); \
  STORE_RESULT; \
} \
NI void prefix##name (int x) \
{ \
  double yy = name ((double)x); \
  STORE_RESULT; \
} \
NI void prefix##name##l (int x) \
{ \
  ldouble yy = name##l ((ldouble)x); \
  STORE_RESULT; \
}
#else
double result;

#define DEF_MATH_FUNC(prefix, name) NI void prefix##name##f (int x) \
{ \
  float yy = name##f ((float) x); \
  STORE_RESULT; \
} \
NI void prefix##name (int x) \
{ \
  double yy = name ((double)x); \
  STORE_RESULT; \
}
#endif

#undef STORE_RESULT
#define STORE_RESULT result = yy
#if defined(GNU_EXTENSION)
DEF_MATH_FUNC (m,pow10)
DEF_MATH_FUNC (m,exp10)
#endif
DEF_MATH_FUNC (m,exp2)
DEF_MATH_FUNC (m,exp)
DEF_MATH_FUNC (m,expm1)
DEF_MATH_FUNC (m,cosh)
DEF_MATH_FUNC (m,sinh)
DEF_MATH_FUNC (m,acos)
DEF_MATH_FUNC (m,asin)
DEF_MATH_FUNC (m,acosh)
DEF_MATH_FUNC (m,atanh)
DEF_MATH_FUNC (m,log)
DEF_MATH_FUNC (m,log2)
DEF_MATH_FUNC (m,log10)
DEF_MATH_FUNC (m,log1p)
DEF_MATH_FUNC (m,sqrt)

#undef STORE_RESULT
#define STORE_RESULT
#if defined(GNU_EXTENSION)
DEF_MATH_FUNC (o,pow10)
DEF_MATH_FUNC (o,exp10)
#endif
DEF_MATH_FUNC (o,exp2)
DEF_MATH_FUNC (o,exp)
DEF_MATH_FUNC (o,expm1)
DEF_MATH_FUNC (o,cosh)
DEF_MATH_FUNC (o,sinh)
DEF_MATH_FUNC (o,acos)
DEF_MATH_FUNC (o,asin)
DEF_MATH_FUNC (o,acosh)
DEF_MATH_FUNC (o,atanh)
DEF_MATH_FUNC (o,log)
DEF_MATH_FUNC (o,log2)
DEF_MATH_FUNC (o,log10)
DEF_MATH_FUNC (o,log1p)
DEF_MATH_FUNC (o,sqrt)

#if defined(LARGE_LONG_DOUBLE)
#define INIT_MATH_FUNC(prefix, name, lb, ub) { prefix##name##f, #name "f", 0, 0, lb, ub }, \
{ prefix##name, #name, 0, 0, lb, ub }, \
{ prefix##name##l, #name "l" , 0, 0, lb, ub }, 
#else
#define INIT_MATH_FUNC(prefix, name, lb, ub) { prefix##name##f, #name "f", 0, 0, lb, ub }, \
{ prefix##name, #name, 0, 0, lb, ub },
#endif

struct MathFuncInfo
{
  FP math_func;
  const char*  name;
  int lb;
  int ub;
  bool has_lb;
  bool has_ub;
} math_func_arr[] = { 
#if defined(GNU_EXTENSION)
  INIT_MATH_FUNC (m,pow10, false, true)
  INIT_MATH_FUNC (m,exp10, false, true)
#endif
  INIT_MATH_FUNC (m,exp2, false, true)
  INIT_MATH_FUNC (m,expm1, false, true)
  INIT_MATH_FUNC (m,exp, false, true)
  INIT_MATH_FUNC (m,cosh, true, true)
  INIT_MATH_FUNC (m,sinh, true, true)
  INIT_MATH_FUNC (m,acos, true, true)
  INIT_MATH_FUNC (m,asin, true, true)
  INIT_MATH_FUNC (m,acosh, true, false)
  INIT_MATH_FUNC (m,atanh, true, true)
  INIT_MATH_FUNC (m,log10, true, false)
  INIT_MATH_FUNC (m,log, true, false)
  INIT_MATH_FUNC (m,log2, true, false)
  INIT_MATH_FUNC (m,log1p, true, false)
  INIT_MATH_FUNC (m,sqrt, true, false)
  { 0, 0,  0, 0, 0, 0} };

MathFuncInfo opt_math_func_arr[] = 
{
#if defined(GNU_EXTENSION)
  INIT_MATH_FUNC (o,pow10, false, true)
  INIT_MATH_FUNC (o,exp10, false, true)
#endif
  INIT_MATH_FUNC (o,exp2, false, true)
  INIT_MATH_FUNC (o,expm1, false, true)
  INIT_MATH_FUNC (o,exp, false, true)
  INIT_MATH_FUNC (o,cosh, true, true)
  INIT_MATH_FUNC (o,sinh, true, true)
  INIT_MATH_FUNC (o,acos, true, true)
  INIT_MATH_FUNC (o,asin, true, true)
  INIT_MATH_FUNC (o,acosh, true, false)
  INIT_MATH_FUNC (o,atanh, true, true)
  INIT_MATH_FUNC (o,log10, true, false)
  INIT_MATH_FUNC (o,log, true, false)
  INIT_MATH_FUNC (o,log2, true, false)
  INIT_MATH_FUNC (o,log1p, true, false)
  INIT_MATH_FUNC (o,sqrt, true, false)
  { 0, 0,  0, 0, 0, 0} };

int test (MathFuncInfo* math_func_infos)
{
  int i = 0;
  int te = 0;

  for (i = 0; math_func_infos[i].math_func; i++)
    {
      MathFuncInfo& info = math_func_infos[i];
      int j;
      if (info.has_lb)
        {
          for (j = 0; j > -500000; j--)
            {
        
              errno = 0;
              info.math_func (j);
              if (errno != 0)
                {
		  te++;
                  info.lb = j ;
                  break;
                }
            }
        }
      if (info.has_ub)
        {
          for (j = 0; j < 500000; j++)
            {
              errno = 0;
              info.math_func (j);
              if (errno != 0)
              {
	        te++;
                info.ub = j ;
                break;
              }
            }
        }
    }
  return te;
}

int main()
{
   int te1, te2;

   te1 = test (&math_func_arr[0]);
   te2 = test (&opt_math_func_arr[0]);

   // Now examine the result 
   int i = 0;
   int errcnt = 0;
   for (i = 0; math_func_arr[i].math_func; i++)
   {
      MathFuncInfo& info = math_func_arr[i];
      MathFuncInfo& opt_info = opt_math_func_arr[i];
#ifdef DEBUG
      fprintf (stderr," %s: lb = %d, ub = %d: lb_opt = %d, ub_opt = %d\n",
	info.name, info.lb, info.ub, opt_info.lb, opt_info.ub);
#endif
      if (info.lb != opt_info.lb) errcnt ++;
      if (info.ub != opt_info.ub) errcnt ++;
   }
   if (errcnt) abort();
   return 0;
}
