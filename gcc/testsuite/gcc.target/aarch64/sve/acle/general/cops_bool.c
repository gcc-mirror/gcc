/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#ifdef __cplusplus
#define BOOL bool
#else
#define BOOL _Bool
#endif

#define DECL_FUNC_UNARY(name, op, intr, n) \
  svbool_t __attribute__((noipa)) \
  func_ ## name ## _unary (svbool_t a) { \
    return op (a); \
  } \
  void __attribute__((noipa)) \
  checkfunc_ ## name ## _unary () { \
    svbool_t pg = svptrue_b8 (); \
    svbool_t data = svptrue_pat_b8 (SV_VL ## n); \
    svbool_t exp = intr (pg, data); \
    svbool_t actual = func_ ## name ## _unary (data); \
    svbool_t cmp = sveor_b_z (pg, exp, actual); \
    if (svptest_any (pg, cmp)) \
      __builtin_abort (); \
  }

#define DECL_FUNC_UNARY_COND(name, op, n)  \
  svbool_t __attribute__ ((noipa)) \
  func_ ## name  ## _unary_cond (svbool_t a) { \
    return op (a); \
  } \
  svbool_t __attribute__ ((noipa)) \
  name (svbool_t t, svbool_t a) { \
    svbool_t pgl = sveor_b_z (t, a, svptrue_b8 ()); \
    return pgl; \
  } \
  void __attribute__((noipa)) \
  checkfunc_ ## name ## _unary_cond () { \
    svbool_t a = svptrue_pat_b8 (SV_VL ## n); \
    svbool_t all_true = svptrue_b8 (); \
    svbool_t cmp = func_ ## name ## _unary_cond (a); \
    svbool_t pgc = name (all_true, a) ; \
    svbool_t res = sveor_b_z (all_true, cmp, pgc); \
    if (svptest_any (all_true, res)) \
      __builtin_abort (); \
  }

#define VECT_CST { -1, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1 } /* { dg-warning "overflow in conversion from" "" { target c } }  */
#define VECT_CSTN { -1, t (), 0, -1, 0, f (), 0, 0, 0, -1, 0, -1, 0, -1, 0, -1 } /* { dg-warning "overflow in conversion from" "" { target c } }  */
	/* { dg-warning "narrowing conversion of" "" { target c++ } .-1 }  */

#define DECL_FUNC_INIT() \
  svbool_t __attribute__ ((noipa)) \
  func_init1 () { \
    svbool_t temp = VECT_CST; \
    return temp; \
  } \
  svbool_t __attribute__ ((noipa)) \
  func_init2 () { \
    svbool_t temp = { 0 }; \
    return temp; \
  } \
  svbool_t __attribute__ ((noipa)) \
  func_init3 () { \
    svbool_t temp = { }; \
    return temp; \
  } \
  int __attribute__ ((noipa)) \
  t () { \
    return -1; \
  } \
  int __attribute__ ((noipa)) \
  f () { \
    return 0; \
  } \
  svbool_t __attribute__ ((noipa)) \
  func_init4 () { \
    svbool_t temp = VECT_CSTN; \
    return temp; \
  } \
  void __attribute__((noipa)) \
  checkfunc_init() { \
    svbool_t all_true = svptrue_b8 (); \
    svbool_t v16_true = svptrue_pat_b8 (SV_VL16); \
    int8_t mem[] = VECT_CST; \
    int8_t memn[] = VECT_CSTN; \
    svint8_t t8 = svld1_s8 (v16_true, mem); \
    svbool_t init1 = __builtin_convertvector (t8, svbool_t); \
    svbool_t init2 = __builtin_convertvector (svindex_s8 (0, 0), svbool_t); \
    svbool_t init3 = __builtin_convertvector (svindex_s8 (0, 0), svbool_t); \
    svint8_t tn8 = svld1_s8 (v16_true, memn); \
    svbool_t init4 = __builtin_convertvector (tn8, svbool_t); \
	\
    svbool_t res_init1 = func_init1 (); \
    svbool_t cmp = sveor_b_z (all_true, init1, res_init1); \
    if (svptest_any (v16_true, cmp)) \
      __builtin_abort (); \
	\
    svbool_t res_init2 = func_init2 (); \
    cmp = sveor_b_z (all_true, init2, res_init2); \
    if (svptest_any (v16_true, cmp)) \
      __builtin_abort (); \
	\
    svbool_t res_init3 = func_init3 (); \
    cmp = sveor_b_z (all_true, init3, res_init3); \
    if (svptest_any (v16_true, cmp)) \
      __builtin_abort (); \
	\
    svbool_t res_init4 = func_init4 (); \
    cmp = sveor_b_z (all_true, init4, res_init4); \
    if (svptest_any (v16_true, cmp)) \
      __builtin_abort (); \
  }

#define DECL_FUNC_BINARY(name, op, intr, n) \
  svbool_t __attribute__((noipa)) \
  func_ ## name ## _binary(svbool_t a, svbool_t b) { \
    return (a) op (b); \
  } \
  void __attribute__((noipa)) \
  checkfunc_ ## name ## _binary () { \
    svbool_t pg = svptrue_b8 (); \
    svbool_t data1 = svptrue_pat_b8 (SV_VL ## n); \
    svbool_t data2 = svnot_b_z (pg, data1); \
    svbool_t exp = intr (pg, data1, data2); \
    svbool_t actual = func_ ## name ## _binary (data1, data2); \
    svbool_t cmp = sveor_b_z (pg, exp, actual); \
    if (svptest_any (pg, cmp)) \
      __builtin_abort (); \
  }

#define DECL_FUNC_BINARY_COND(name, op, intr, n) \
  svbool_t __attribute__ ((noipa)) \
  func_ ## name ## _binary_cond(svbool_t a, svbool_t b) { \
    return (a) op (b); \
  } \
  svbool_t __attribute__ ((noipa)) \
  name ## intr (svbool_t t, svbool_t a, svbool_t b) { \
    return sv ## intr ## _b_z (t, a, b); \
  } \
  void __attribute__((noipa)) \
  checkfunc_ ## name ## _binary_cond () { \
    svbool_t all_true = svptrue_b8 (); \
    svbool_t a = svptrue_pat_b8 (SV_VL ## n); \
    svbool_t b = svnot_b_z (all_true, a); \
    svbool_t cmp = func_ ## name ## _binary_cond (a, b); \
    svbool_t pgc = name ## intr (all_true, a, b) ; \
    svbool_t res = sveor_b_z (all_true, cmp, pgc); \
    if (svptest_any (all_true, res)) \
      __builtin_abort (); \
  }

#define DECL_FUNC_BOOL_TERNARY(n) \
  svbool_t __attribute__((noipa)) \
  func_svbool_t_ternary_eq(svbool_t p, svbool_t q, svbool_t a, svbool_t b) { \
    return (p == q) ? a : b; \
  } \
  svbool_t __attribute__((noipa)) \
  func_svbool_t_ternary_ne(svbool_t p, svbool_t q, svbool_t a, svbool_t b) { \
    return (p != q) ? a : b; \
  } \
  svbool_t __attribute__((noipa)) \
  func_svbool_t_ternary_ex(svbool_t p, svbool_t a, svbool_t b) { \
    return (p) ? a : b; \
  } \
  void __attribute__((noipa)) \
  checkfunc_svbool_t_ternary () { \
    svbool_t pg = svptrue_b8 (); \
    svbool_t p = svptrue_pat_b8 (SV_VL ## n); \
    svbool_t q = svnot_b_z (pg, p); \
    svbool_t a = p; \
    svbool_t b = q; \
    svbool_t ne = sveor_b_z (pg, p, q); \
    svbool_t eq = svnot_b_z (pg, sveor_b_z (pg, p, q)); \
    svbool_t ex = p; \
    svbool_t expeq = svsel_b (eq, a, b); \
    svbool_t expne = svsel_b (ne, a, b); \
    svbool_t expex = svsel_b (ex, a, b); \
    svbool_t actualeq = func_svbool_t_ternary_eq (p, q, a, b); \
    svbool_t actualne = func_svbool_t_ternary_ne (p, q, a, b); \
    svbool_t actualex = func_svbool_t_ternary_ex (p, a, b); \
    svbool_t pgc_eq = sveor_b_z (pg, actualeq, expeq); \
    svbool_t pgc_ne = sveor_b_z (pg, actualne, expne); \
    svbool_t pgc_ex = sveor_b_z (pg, actualex, expex); \
    if (svptest_any (pg, pgc_eq)) \
      __builtin_abort (); \
    if (svptest_any (pg, pgc_ne)) \
      __builtin_abort (); \
    if (svptest_any (pg, pgc_ex)) \
      __builtin_abort (); \
  }

svbool_t __attribute__((noipa))
my_svneor_b_z (svbool_t pg, svbool_t a, svbool_t b)
{
  return svnot_b_z (pg, sveor_b_z (pg, a, b));
}

svbool_t  __attribute__((noipa))
func_svbool_t_bc (svint8_t a)
{
  return __builtin_convertvector (a, svbool_t);
}

void __attribute__((noipa))
checkfunc_svbool_t_bc ()
{
  svbool_t pg = svptrue_b8 ();
  svint8_t data = { -1, -1, -1, -1 };
  svbool_t actual = func_svbool_t_bc (data);
  svbool_t exp = svptrue_pat_b8 (SV_VL4);
  svbool_t cmp = sveor_b_z (pg, exp, actual);
  if (svptest_any (pg, cmp))
    __builtin_abort ();
}

svint8_t  __attribute__((noipa))
func_svint8_t_bc (svbool_t a)
{
  return __builtin_convertvector (a, svint8_t);
}

void __attribute__((noipa))
checkfunc_svint8_t_bc ()
{
  svbool_t pg = svptrue_b8 ();
  svbool_t data = svptrue_pat_b8 (SV_VL4);
  svint8_t actual = func_svint8_t_bc (data);
  svint8_t exp = { -1, -1, -1, -1 };
  svbool_t cmp = svcmpne_s8 (pg, exp, actual);
  if (svptest_any (pg, cmp))
    __builtin_abort ();
}

DECL_FUNC_UNARY (not, ~, svnot_b_z, 4)
DECL_FUNC_BINARY (and, &, svand_b_z, 8)
DECL_FUNC_BINARY (orr, |, svorr_b_z, 6)
DECL_FUNC_BINARY (eor, ^, sveor_b_z, 3)

DECL_FUNC_BINARY (eq, ==, my_svneor_b_z, 4)
DECL_FUNC_BINARY (ne, !=, sveor_b_z, 4)

DECL_FUNC_INIT ()

#ifdef __cplusplus
DECL_FUNC_UNARY_COND (lnot, !, 8)
DECL_FUNC_BINARY_COND (and_and, &&, and, 8)
DECL_FUNC_BINARY_COND (or_or, ||, orr, 8)
DECL_FUNC_BOOL_TERNARY (3)
#endif

#undef DECL_FUNC_UNARY
#define DECL_FUNC_UNARY(name, op, intr, n) \
  checkfunc_ ## name ## _unary ();

#undef DECL_FUNC_UNARY_COND
#define DECL_FUNC_UNARY_COND(name, op, n)  \
  checkfunc_ ## name ## _unary_cond ();

#undef DECL_FUNC_INIT
#define DECL_FUNC_INIT() \
  checkfunc_init ();

#undef DECL_FUNC_BINARY
#define DECL_FUNC_BINARY(name, op, intr, n) \
  checkfunc_ ## name ## _binary ();

#undef DECL_FUNC_BINARY_COND
#define DECL_FUNC_BINARY_COND(name, op, intr, n) \
  checkfunc_ ## name ## _binary_cond ();

#undef  DECL_FUNC_BOOL_TERNARY
#define  DECL_FUNC_BOOL_TERNARY(n) \
  checkfunc_svbool_t_ternary ();

int
main ()
{
  DECL_FUNC_UNARY (not, ~, svnot_b_z, 4)
  DECL_FUNC_BINARY (and, &, svand_b_z, 8)
  DECL_FUNC_BINARY (orr, |, svorr_b_z, 6)
  DECL_FUNC_BINARY (eor, ^, sveor_b_z, 3)

  DECL_FUNC_BINARY (ne, !=, sveor_b_z, 4)
  DECL_FUNC_BINARY (eq, ==, my_svneor_b_z, 4)

  DECL_FUNC_INIT ()

  checkfunc_svbool_t_bc ();
  checkfunc_svint8_t_bc ();

#ifdef __cplusplus
  DECL_FUNC_UNARY_COND (lnot, !, 8)
  DECL_FUNC_BINARY_COND (and_and, &&, and, 8)
  DECL_FUNC_BINARY_COND (or_or, ||, orr, 8)
  DECL_FUNC_BOOL_TERNARY (3)
#endif

  return 0;
}
