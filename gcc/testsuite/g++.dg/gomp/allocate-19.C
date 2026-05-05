/* { dg-do compile { target c++14 } } */
/* { dg-additional-options "-fimplicit-constexpr" } */

/* OpenMP allocate directive in constant expressions where execution does not
   pass through the allocation of the variable in the directive.
   Regular functions and function templates,
   constexpr and inline with -fimplicit-constexpr.

   These cases will be valid if/when OpenMP relaxes restrictions on directives
   in constexpr functions.  It might make sense to only allow this behavior in
   c++23 though.

   It doesn't make sense to test these cases in c++11 as constexpr functions
   are far more limited, and are diagnosed completely differently.

   Even though -fimplicit-constexpr is an extension, its behavior is similar to
   lambdas in c++17, so I am including tests for it.
   See allocate-18.C for test cases involving lambdas.
   
   NOTE: The error messages for the inline cases are are probably not what we
   want, we likely want something stating that the calls are not usable in a
   constant expression because of the use of an OpenMP directive.  */

constexpr int f_constexpr(bool b)
{
  if (b)
    return 42;
  int a = 42;
  #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
  return a;
}
constexpr int g_cx_0 = f_constexpr(true); /* { dg-error "'constexpr int f_constexpr\\\(bool\\\)' called in a constant expression" "" { xfail *-*-* } } */

template<typename>
constexpr int f_constexpr_templ_uninstantiated(bool b)
{
  if (b)
    return 42;
  int a = 42;
  #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
  return a;
}

template<typename>
constexpr int f_constexpr_templ(bool b)
{
  if (b)
    return 42;
  int a = 42;
  #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
  return a;
}
constexpr int g_cx_1 = f_constexpr_templ<void>(true); /* { dg-error "'constexpr int f_constexpr_templ\\\(bool\\\) \\\[with <template-parameter-1-1> = void\\\]' called in a constant expression" "" { xfail *-*-* } } */

template<typename B>
constexpr int f_constexpr_dep_parm_templ_uninstantiated(B b)
{
  if (b)
    return 42;
  int a = 42;
  #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
  return a;
}

template<typename B>
constexpr int f_constexpr_dep_parm_templ(B b)
{
  if (b)
    return 42;
  int a = 42;
  #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" } */
  return a;
}
constexpr int g_cx_2 = f_constexpr_dep_parm_templ(true); /* { dg-error "'constexpr int f_constexpr_dep_parm_templ\\\(bool\\\) \\\[with <template-parameter-1-1> = bool\\\]' called in a constant expression" "" { xfail *-*-* } } */



inline int f_inline(bool b)
{
  if (b)
    return 42;
  int a = 42;
  #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
  return a;
}
constexpr int g_inline_0 = f_inline(true);  /* { dg-error "'int f_inline\\\(bool\\\)' called in a constant expression" "" { xfail *-*-* } } */

template<typename>
inline int f_inline_templ_uninstantiated(bool b)
{
  if (b)
    return 42;
  int a = 42;
  #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
  return a;
}

template<typename>
inline int f_inline_templ(bool b)
{
  if (b)
    return 42;
  int a = 42;
  #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
  return a;
}
constexpr int g_inline_1 = f_inline_templ<void>(true);  /* { dg-error "'int f_inline_templ\\\(bool\\\) \\\[with <template-parameter-1-1> = void\\\]' called in a constant expression" "" { xfail *-*-* } } */

template<typename B>
inline int f_inline_dep_parm_templ_uninstantiated(B b)
{
  if (b)
    return 42;
  int a = 42;
  #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
  return a;
}

template<typename B>
inline int f_inline_dep_parm_templ(B b)
{
  if (b)
    return 42;
  int a = 42;
  #pragma omp allocate(a) /* { dg-error "OpenMP directives may not appear in 'constexpr' functions" "" { xfail *-*-* } } */
  return a;
}
constexpr int g_inline_2 = f_inline_dep_parm_templ(true);  /* { dg-error "'int f_inline_deb_parm_templ\\\(bool\\\) \\\[with <template-parameter-1-1> = bool\\\]' called in a constant expression" "" { xfail *-*-* } } */
