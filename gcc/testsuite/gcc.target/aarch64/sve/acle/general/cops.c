/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#include <arm_sve.h>
#include <stdio.h>

#define DECL_FUNC_UNARY(type, name, op, intr, su, sz, id) \
  __attribute__ ((noipa)) \
  type func_ ## name ## type ## _unary (type a) { \
    return op (a); \
  } \
  void checkfunc_ ## name ## type ## _unary () { \
    type data = svindex_ ## su ## sz (0, 1); \
    type zr = svindex_ ## su ## sz (0, 0); \
    type one = svindex_ ## su ## sz (1, 0); \
    type mone = svindex_ ## su ## sz (-1, 0); \
    svbool_t pg = svptrue_b ## sz (); \
    type exp = intr ## su ## sz ## _z (pg, id, data); \
    type actual = func_ ## name ## type ## _unary (data); \
    svbool_t res = svcmpeq_ ## su ## sz (pg, exp, actual); \
    if (svptest_any (pg, svnot_b_z (pg, res))) \
      __builtin_abort (); \
  }

#define DECL_FUNC_UNARY_FLOAT(type, name, op, intr, su, sz, id) \
  __attribute__ ((noipa)) \
  type func_ ## name ## type ## _unary (type a) { \
    return op (a); \
  } \
  void checkfunc_ ## name ## type ## _unary () { \
    type data = svdup_n_ ## su ## sz (2.0); \
    type zr = svdup_n_ ## su ## sz (0.0); \
    type one = svdup_n_ ## su ## sz (1.0); \
    type mone = svdup_n_ ## su ## sz (-1.0); \
    svbool_t pg = svptrue_b ## sz (); \
    type exp = intr ## su ## sz ## _z (pg, id, data); \
    type actual = func_ ## name ## type ## _unary (data); \
    svbool_t res = svcmpeq_ ## su ## sz (pg, exp, actual); \
    if (svptest_any (pg, svnot_b_z (pg, res))) \
      __builtin_abort (); \
  }

#define DECL_FUNC_INDEX(rtype, type, intr, su, sz)  \
  __attribute__ ((noipa)) \
  rtype func_ ## rtype ## type ## _vindex (type a, int n) { \
    return (a[n]); \
  } \
  __attribute__ ((noipa)) \
  rtype func_ ## rtype ## type ## _cindex (type a) { \
    return (a[0]); \
  } \
  void checkfunc_ ## rtype ## type ## _vindex () { \
    type a = svindex_ ## su ## sz (0, 1); \
    int n = 2; \
    if (2 != func_ ## rtype ## type ## _vindex (a, n)) \
      __builtin_abort (); \
  } \
  void checkfunc_ ## rtype ## type ## _cindex () { \
    type a = svindex_ ## su ## sz (1, 0); \
    if (1 != func_ ## rtype ## type ## _cindex (a)) \
      __builtin_abort (); \
  }

#define DECL_FUNC_INDEX_FLOAT(rtype, type, intr, su, sz)  \
  __attribute__ ((noipa)) \
  rtype func_ ## rtype ## type ## _vindex (type a, int n) { \
    return (a[n]); \
  } \
  __attribute__ ((noipa)) \
  rtype func_ ## rtype ## type ## _cindex (type a) { \
    return (a[0]); \
  } \
  void checkfunc_ ## rtype ## type ## _vindex () { \
    type a = svdup_n_ ## su ## sz (2.0); \
    int n = 2; \
    if (2.0 != func_ ## rtype ## type ## _vindex (a, n)) \
      __builtin_abort (); \
  } \
  void checkfunc_ ## rtype ## type ## _cindex () { \
    type a = svdup_n_ ## su ## sz (4.0); \
    if (4.0 != func_ ## rtype ## type ## _cindex (a)) \
      __builtin_abort (); \
  }

#define DECL_FUNC_BINARY(type, name, op, intr, su, sz)  \
  __attribute__ ((noipa)) \
  type func_ ## name  ## type ## _binary(type a, type b) { \
    return (a) op (b); \
  } \
  void checkfunc_ ## name ## type ## _binary () { \
    type a = svindex_ ## su ## sz (0, 1); \
    type b = svindex_ ## su ## sz (0, 2); \
    svbool_t all_true = svptrue_b ## sz (); \
    type exp = intr ## su ## sz ## _z (all_true, a, b); \
    type actual = func_ ## name ## type ## _binary (a, b); \
    svbool_t res = svcmpeq_ ## su ## sz (all_true, exp, actual); \
    if (svptest_any (all_true, svnot_b_z (all_true, res))) \
      __builtin_abort (); \
  }

#define DECL_FUNC_BINARY_SHIFT(type, name, op, intr, su, sz)  \
  __attribute__ ((noipa)) \
  type func_ ## name  ## type ## _binary(type a, type b) { \
    return (a) op (b); \
  } \
  void checkfunc_ ## name ## type ## _binary () { \
    type a = svindex_ ## su ## sz (0, 1); \
    svuint ## sz ## _t b = svindex_u ## sz (0, 2); \
    type c = svindex_ ## su ## sz (0, 2); \
    svbool_t all_true = svptrue_b ## sz (); \
    type exp = intr ## su ## sz ## _z (all_true, a, b); \
    type actual = func_ ## name ## type ## _binary (a, c); \
    svbool_t res = svcmpeq_ ## su ## sz (all_true, exp, actual); \
    if (svptest_any (all_true, svnot_b_z (all_true, res))) \
      __builtin_abort (); \
  }

#define DECL_FUNC_BINARY_FLOAT(type, name, op, intr, su, sz)  \
  __attribute__ ((noipa)) \
  type func_ ## name  ## type ## _binary(type a, type b) { \
    return (a) op (b); \
  } \
  void checkfunc_ ## name ## type ## _binary () { \
    type a = svdup_n_ ## su ## sz (1.0); \
    type b = svdup_n_ ## su ## sz (2.0); \
    svbool_t all_true = svptrue_b ## sz (); \
    type exp = intr ## su ## sz ## _z (all_true, a, b); \
    type actual = func_ ## name ## type ## _binary (a, b); \
    svbool_t res = svcmpeq_ ## su ## sz (all_true, exp, actual); \
    if (svptest_any (all_true, svnot_b_z (all_true, res))) \
      __builtin_abort (); \
  }

#define DECL_FUNC_BINARY_CMP(type, name, op, intr, su, sz)  \
  __attribute__ ((noipa)) \
  type func_ ## name  ## type ## _binary_cmp(type a, type b) { \
    return (a) op (b); \
  } \
  void checkfunc_ ## name ## type ## _binary_cmp () { \
    type a = svindex_ ## su ## sz (0, 1); \
    type b = svindex_ ## su ## sz (0, 2); \
    type c = func_ ## name ## type ## _binary_cmp (a, b); \
    svbool_t all_true = svptrue_b ## sz (); \
    svbool_t pgc = svcmpne_n_ ## su ## sz (all_true, c, 0); \
    svbool_t pg = intr ## su ## sz (all_true, a, b); \
    svbool_t res = sveor_b_z (all_true, pgc, pg); \
    if (svptest_any (all_true, res)) \
      __builtin_abort (); \
  }

#define DECL_FUNC_TERNARY(type, su, sz) \
  __attribute__ ((noipa)) \
  type func_ ## type ## _ternary(type p, type q, type a, type b) { \
    return (p >= q) ? a : b; \
  } \
  void checkfunc_ ## type ## _ternary() { \
    type p = svindex_ ## su ## sz (0, 1); \
    type q = svindex_ ## su ## sz (1, 1); \
    type a = svindex_ ## su ## sz (2, 1); \
    type b = svindex_ ## su ## sz (3, 1); \
    svbool_t all_true = svptrue_b ## sz (); \
    svbool_t cmp = svcmpge_ ## su ## sz (all_true, p, q); \
    type res = svsel_ ## su ## sz (cmp, a, b); \
    type actual = func_ ## type ## _ternary(p, q, a, b); \
    svbool_t pgc = svcmpeq_ ## su ## sz (all_true, res, actual); \
    if (svptest_any (all_true, svnot_b_z (all_true, pgc))) \
      __builtin_abort (); \
  }

#define VECT_CST_s8  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 }
#define VECT_CST_s16 { 0, 1, 2, 3, 4, 5, 6, 7 }
#define VECT_CST_s32 { 0, 1, 2, 3 }
#define VECT_CST_s64 { 0, 1 }

#define VECT_CSTN_s8  { 0, foo_s8 (), 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 }
#define VECT_CSTN_s16 { 0, foo_s16 (), 2, 3, 4, 5, 6, 7 }
#define VECT_CSTN_s32 { 0, foo_s32 (), 2, 3 }
#define VECT_CSTN_s64 { 0, foo_s64 () }

#define DECL_FUNC_INIT(type, su, sz, nelem) \
  __attribute__ ((noipa)) \
  type func_ ## type ## _init1() { \
    type temp = VECT_CST_ ## su ## sz; \
    return temp; \
  } \
  __attribute__ ((noipa)) \
  type func_ ## type ## _init2() { \
    type temp = { 0 }; \
    return temp; \
  } \
  __attribute__ ((noipa)) \
  type func_ ## type ## _init3() { \
    type temp = { }; \
    return temp; \
  } \
  __attribute__ ((noipa)) \
  int ## sz ## _t foo_ ## su ## sz () { \
    return 1; \
  } \
  __attribute__ ((noipa)) \
  type func_ ## type ## _init4() { \
    type temp = VECT_CSTN_ ## su ## sz; \
    return temp; \
  } \
  void checkfunc_ ## type ## _init() { \
    svbool_t all_true = svptrue_b ## sz (); \
    int ## sz ## _t mem[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 }; \
    svbool_t cmp ## sz = svptrue_pat_b ## sz (SV_VL ## nelem); \
    type init1 = svld1_ ## su ## sz (cmp ## sz, mem); \
    type init2 = svindex_ ## su ## sz (0, 0); \
    type init3 = svindex_ ## su ## sz (0, 0); \
    type init4 = svld1_ ## su ## sz (cmp ## sz, mem); \
	\
    type res_init1 = func_ ## type ## _init1 (); \
    svbool_t cmp = svcmpne_ ## su ## sz (cmp ## sz, init1, res_init1); \
    if (svptest_any (all_true, cmp)) \
      __builtin_abort (); \
	\
    type res_init2 = func_ ## type ## _init2 (); \
    cmp = svcmpne_ ## su ## sz (cmp ## sz, init2, res_init2); \
    if (svptest_any (all_true, cmp)) \
      __builtin_abort (); \
	\
    type res_init3 = func_ ## type ## _init3 (); \
    cmp = svcmpne_ ## su ## sz (cmp ## sz, init3, res_init3); \
    if (svptest_any (all_true, cmp)) \
      __builtin_abort (); \
	\
    type res_init4 = func_ ## type ## _init4 (); \
    cmp = svcmpne_ ## su ## sz (cmp ## sz, init4, res_init4); \
    if (svptest_any (all_true, cmp)) \
      __builtin_abort (); \
  }

#define DECL_FUNC_TERNARY_FLOAT(type, su, sz) \
  __attribute__ ((noipa)) \
  type func_ ## type ## _ternary(type p, type q, type a, type b) { \
    return (p >= q) ? a : b; \
  } \
  void checkfunc_ ## type ## _ternary() { \
    type p = svdup_n_ ## su ## sz (2.0); \
    type q = svdup_n_ ## su ## sz (1.0); \
    type a = svdup_n_ ## su ## sz (3.0); \
    type b = svdup_n_ ## su ## sz (4.0); \
    svbool_t all_true = svptrue_b ## sz (); \
    svbool_t cmp = svcmpge_ ## su ## sz (all_true, p, q); \
    type res = svsel_ ## su ## sz (cmp, a, b); \
    type actual = func_ ## type ## _ternary(p, q, a, b); \
    svbool_t pgc = svcmpeq_ ## su ## sz (all_true, res, actual); \
    if (svptest_any (all_true, svnot_b_z (all_true, pgc))) \
      __builtin_abort (); \
  }

#define DECL_FUNC_BINARY_SCALAR(scalar, type, name, op, intr, su, sz)  \
  __attribute__ ((noipa)) \
  type func_ ## name  ## type ## _binary_cscalar(type a) { \
    return (a) op scalar; \
  } \
  __attribute__ ((noipa)) \
  type func_ ## name  ## type ## _binary_vscalar(type a, int b) { \
    return (a) op b; \
  } \
  void checkfunc_ ## name ## type ## _binary_cscalar () { \
    type a = svindex_ ## su ## sz (0, 1); \
    svbool_t pg = svptrue_b ## sz (); \
    type exp = intr ## su ## sz ## _z (pg, a, scalar); \
    type actual = func_ ## name ## type ## _binary_cscalar (a); \
    svbool_t res = svcmpeq_ ## su ## sz (pg, exp, actual); \
    if (svptest_any (pg, svnot_b_z (pg, res))) \
      __builtin_abort (); \
  } \
  void checkfunc_ ## name ## type ## _binary_vscalar () { \
    type a = svindex_ ## su ## sz (0, 1); \
    svbool_t pg = svptrue_b ## sz (); \
    uint ## sz ## _t b = scalar; \
    type exp = intr ## su ## sz ## _z (pg, a, b); \
    type actual = func_ ## name ## type ## _binary_vscalar (a, b); \
    svbool_t res = svcmpeq_ ## su ## sz (pg, exp, actual); \
    if (svptest_any (pg, svnot_b_z (pg, res))) \
      __builtin_abort (); \
  }

#define DECL_FUNC_UNARY_RET_FLOAT(rtype, type, name, op, intr, su, sz)  \
  __attribute__ ((noipa)) \
  rtype func_ ## name  ## rtype ## type ## _unary(type a) { \
    return op (a); \
  } \
  void checkfunc_ ## name ## rtype ## type ## _unary () { \
    type a = svdup_n_ ## su ## sz (2.0); \
    svbool_t pg = svptrue_b ## sz (); \
    rtype exp = intr ## su ## sz ## _z (a); \
    rtype actual = func_ ## name ## type ## _unary (a); \
    svbool_t res = svcmpeq_ ## su ## sz (pg, exp, actual); \
    if (svptest_any (pg, svnot_b_z (pg, res))) \
      __builtin_abort (); \
  }

#define DECL_FUNC_BINARY_RET_FLOAT_CMP(rtype, type, name, op, intr, su, sz)  \
  __attribute__ ((noipa)) \
  rtype func_ ## name  ## rtype ## type ## _binary_cmp(type a, type b) { \
    return (a) op (b); \
  } \
  void checkfunc_ ## name ## rtype ## type ## _binary_cmp () { \
    type a = svdup_n_ ## su ## sz (2.0); \
    type b = svdup_n_ ## su ## sz (3.0); \
    svbool_t all_true = svptrue_b ## sz (); \
    rtype cmp = func_ ## name ## rtype ## type ## _binary_cmp (a, b); \
    svbool_t pgc = intr ## su ## sz (all_true, a, b) ; \
    svbool_t pg = svcmpne_n_s ## sz (all_true, cmp, 0); \
    svbool_t res = sveor_b_z (all_true, pg, pgc); \
    if (svptest_any (all_true, res)) \
      __builtin_abort (); \
  }

#define DECL_FUNC_BINARY_RET_COND(rtype, type, name, op, intr, su, sz)  \
  __attribute__ ((noipa)) \
  rtype func_ ## name  ## rtype ## type ## _binary_cond(type a, type b) { \
    return (a) op (b); \
  } \
  __attribute__ ((noipa)) \
  svbool_t name ## intr ## su ## sz (svbool_t t, type a, type b) { \
    svbool_t pgl = svcmpne_n_ ## su ## sz (t, a, 0); \
    svbool_t pgr = svcmpne_n_ ## su ## sz (t, b, 0); \
    return intr ## b_z (t, pgl, pgr); \
  } \
  void checkfunc_ ## name ## rtype ## type ## _binary_cond () { \
    type a = svdup_n_ ## su ## sz (2); \
    type b = svdup_n_ ## su ## sz (3); \
    svbool_t all_true = svptrue_b ## sz (); \
    rtype cmp = func_ ## name ## rtype ## type ## _binary_cond (a, b); \
    svbool_t pgc = name ## intr ## su ## sz (all_true, a, b) ; \
    svbool_t pg = svcmpne_n_s ## sz (all_true, cmp, 0); \
    svbool_t res = sveor_b_z (all_true, pg, pgc); \
    if (svptest_any (all_true, res)) \
      __builtin_abort (); \
  }

#define DECL_FUNC_UNARY_RET_COND(rtype, type, name, op, intr, su, sz)  \
  __attribute__ ((noipa)) \
  rtype func_ ## name  ## rtype ## type ## _unary_cond (type a) { \
    return op (a); \
  } \
  __attribute__ ((noipa)) \
  svbool_t name ## intr ## su ## sz (svbool_t t, type a) { \
    svbool_t pgl = svcmpeq_n_ ## su ## sz (t, a, 0); \
    return pgl; \
  } \
  void checkfunc_ ## name ## rtype ## type ## _unary_cond () { \
    type a = svdup_n_ ## su ## sz (2); \
    svbool_t all_true = svptrue_b ## sz (); \
    rtype cmp = func_ ## name ## rtype ## type ## _unary_cond (a); \
    svbool_t pgc = name ## intr ## su ## sz (all_true, a) ; \
    svbool_t pg = svcmpne_n_s ## sz (all_true, cmp, 0); \
    svbool_t res = sveor_b_z (all_true, pg, pgc); \
    if (svptest_any (all_true, res)) \
      __builtin_abort (); \
  }

#define TYPES_UNARY(name, op, intr, id) \
  DECL_FUNC_UNARY (svint8_t, name, op, intr, s, 8, id) \
  DECL_FUNC_UNARY (svint16_t, name, op, intr, s, 16, id) \
  DECL_FUNC_UNARY (svint32_t, name, op, intr, s, 32, id) \
  DECL_FUNC_UNARY (svint64_t, name, op, intr, s, 64, id) \
  DECL_FUNC_UNARY_FLOAT (svfloat32_t, name, op, intr, f, 32, id) \
  DECL_FUNC_UNARY_FLOAT (svfloat64_t, name, op, intr, f, 64, id)

#define TYPES_INT_UNARY(name, op, intr, id) \
  DECL_FUNC_UNARY (svint8_t, name, op, intr, s, 8, id) \
  DECL_FUNC_UNARY (svint16_t, name, op, intr, s, 16, id) \
  DECL_FUNC_UNARY (svint32_t, name, op, intr, s, 32, id) \
  DECL_FUNC_UNARY (svint64_t, name, op, intr, s, 64, id)

#define TYPES_BINARY(name, op, intr) \
  DECL_FUNC_BINARY (svint8_t, name, op, intr, s, 8) \
  DECL_FUNC_BINARY (svint16_t, name, op, intr, s, 16) \
  DECL_FUNC_BINARY (svint32_t, name, op, intr, s, 32) \
  DECL_FUNC_BINARY (svint64_t, name, op, intr, s, 64) \
  DECL_FUNC_BINARY_FLOAT (svfloat32_t, name, op, intr, f, 32) \
  DECL_FUNC_BINARY_FLOAT (svfloat64_t, name, op, intr, f, 64)

#define TYPES_BINARY32_64(name, op, intr) \
  DECL_FUNC_BINARY (svint32_t, name, op, intr, s, 32) \
  DECL_FUNC_BINARY (svint64_t, name, op, intr, s, 64)

#define TYPES_TERNARY() \
  DECL_FUNC_TERNARY (svint8_t, s, 8) \
  DECL_FUNC_TERNARY (svint16_t, s, 16) \
  DECL_FUNC_TERNARY (svint32_t, s, 32) \
  DECL_FUNC_TERNARY (svint64_t, s, 64) \
  DECL_FUNC_TERNARY_FLOAT (svfloat32_t, f, 32) \
  DECL_FUNC_TERNARY_FLOAT (svfloat64_t, f, 64)

#define TYPES_INDEX(intr) \
  DECL_FUNC_INDEX (int8_t, svint8_t, intr, s, 8) \
  DECL_FUNC_INDEX (int16_t, svint16_t, intr, s, 16) \
  DECL_FUNC_INDEX (int32_t, svint32_t, intr, s, 32) \
  DECL_FUNC_INDEX (int64_t, svint64_t, intr, s, 64) \
  DECL_FUNC_INDEX_FLOAT (float32_t, svfloat32_t, intr, f, 32) \
  DECL_FUNC_INDEX_FLOAT (float64_t, svfloat64_t, intr, f, 64)

#define TYPES_INT_BINARY(name, op, intr) \
  DECL_FUNC_BINARY (svint8_t, name, op, intr, s, 8) \
  DECL_FUNC_BINARY (svint16_t, name, op, intr, s, 16) \
  DECL_FUNC_BINARY (svint32_t, name, op, intr, s, 32) \
  DECL_FUNC_BINARY (svint64_t, name, op, intr, s, 64)

#define TYPES_INT_BINARY_SHIFT(name, op, intr) \
  DECL_FUNC_BINARY_SHIFT (svint8_t, name, op, intr, s, 8) \
  DECL_FUNC_BINARY_SHIFT (svint16_t, name, op, intr, s, 16) \
  DECL_FUNC_BINARY_SHIFT (svint32_t, name, op, intr, s, 32) \
  DECL_FUNC_BINARY_SHIFT (svint64_t, name, op, intr, s, 64)

#define TYPES_INT_BINARY_CMP(name, op, intr) \
  DECL_FUNC_BINARY_CMP (svint8_t, name, op, intr, s, 8) \
  DECL_FUNC_BINARY_CMP (svint16_t, name, op, intr, s, 16) \
  DECL_FUNC_BINARY_CMP (svint32_t, name, op, intr, s, 32) \
  DECL_FUNC_BINARY_CMP (svint64_t, name, op, intr, s, 64) \

#define TYPES_INT_BINARY_SCALAR(c, name, op, intr) \
  DECL_FUNC_BINARY_SCALAR (c, svint8_t, name, op, intr, s, 8) \
  DECL_FUNC_BINARY_SCALAR (c, svint16_t, name, op, intr, s, 16) \
  DECL_FUNC_BINARY_SCALAR (c, svint32_t, name, op, intr, s, 32) \
  DECL_FUNC_BINARY_SCALAR (c, svint64_t, name, op, intr, s, 64)

#define TYPES_FLOAT_BINARY_RET_CMP(name, op, intr) \
  DECL_FUNC_BINARY_RET_FLOAT_CMP (svint32_t, svfloat32_t, name, op, intr, f, 32) \
  DECL_FUNC_BINARY_RET_FLOAT_CMP (svint64_t, svfloat64_t, name, op, intr, f, 64)

#define TYPES_FLOAT_UNARY_RET(name, op, intr) \
  DECL_FUNC_UNARY_RET_FLOAT (svint32_t, svfloat32_t, name, op, intr, f, 32) \
  DECL_FUNC_UNARY_RET_FLOAT (svint64_t, svfloat64_t, name, op, intr, f, 64)

#define TYPES_BINARY_RET_COND(name, op, intr) \
  DECL_FUNC_BINARY_RET_COND (svint8_t, svint8_t, name, op, intr, s, 8) \
  DECL_FUNC_BINARY_RET_COND (svint16_t, svint16_t, name, op, intr, s, 16) \
  DECL_FUNC_BINARY_RET_COND (svint32_t, svint32_t, name, op, intr, s, 32) \
  DECL_FUNC_BINARY_RET_COND (svint64_t, svint64_t, name, op, intr, s, 64) \
  DECL_FUNC_BINARY_RET_COND (svint32_t, svfloat32_t, name, op, intr, f, 32) \
  DECL_FUNC_BINARY_RET_COND (svint64_t, svfloat64_t, name, op, intr, f, 64)

#define TYPES_UNARY_RET_COND(name, op, intr) \
  DECL_FUNC_UNARY_RET_COND (svint8_t, svint8_t, name, op, intr, s, 8) \
  DECL_FUNC_UNARY_RET_COND (svint16_t, svint16_t, name, op, intr, s, 16) \
  DECL_FUNC_UNARY_RET_COND (svint32_t, svint32_t, name, op, intr, s, 32) \
  DECL_FUNC_UNARY_RET_COND (svint64_t, svint64_t, name, op, intr, s, 64) \
  DECL_FUNC_UNARY_RET_COND (svint32_t, svfloat32_t, name, op, intr, f, 32) \
  DECL_FUNC_UNARY_RET_COND (svint64_t, svfloat64_t, name, op, intr, f, 64)

#define TYPES_INIT() \
  DECL_FUNC_INIT (svint8_t, s, 8, 16) \
  DECL_FUNC_INIT (svint16_t, s, 16, 8) \
  DECL_FUNC_INIT (svint32_t, s, 32, 4) \
  DECL_FUNC_INIT (svint64_t, s, 64, 2)

#define C_TYPES \
    TYPES_INIT () \
    TYPES_UNARY (plus, +, svadd_, zr) \
    TYPES_UNARY (minus, -, svsub_, zr) \
    TYPES_UNARY (plus_plus, ++, svadd_, one) \
    TYPES_UNARY (minus_minus, --, svadd_, mone) \
    TYPES_INDEX (svget_lane_) \
    TYPES_BINARY (plus, +, svadd_) \
    TYPES_BINARY (minus, -, svsub_) \
    TYPES_BINARY (mult, *, svmul_) \
    TYPES_BINARY32_64 (div, /, svdiv_) \
    TYPES_INT_BINARY (and, &, svand_) \
    TYPES_INT_BINARY (orr, |, svorr_) \
    TYPES_INT_UNARY (not, ~, sveor_, mone) \
    TYPES_INT_BINARY (eor, ^, sveor_) \
    TYPES_INT_BINARY_SHIFT (lshift, <<, svlsl_) \
    TYPES_INT_BINARY_SHIFT (rshift, >>, svasr_) \
    TYPES_INT_BINARY_SCALAR (2, lshift, <<, svlsl_n_) \
    TYPES_INT_BINARY_SCALAR (2, rshift, >>, svasr_n_) \
    TYPES_INT_BINARY_CMP (eq, ==, svcmpeq_) \
    TYPES_INT_BINARY_CMP (ne, !=, svcmpne_) \
    TYPES_INT_BINARY_CMP (gt, >, svcmpgt_) \
    TYPES_INT_BINARY_CMP (lt, <, svcmplt_) \
    TYPES_INT_BINARY_CMP (ge, >=, svcmpge_) \
    TYPES_INT_BINARY_CMP (le, <=, svcmple_) \
    TYPES_FLOAT_BINARY_RET_CMP (eq, ==, svcmpeq_) \
    TYPES_FLOAT_BINARY_RET_CMP (ne, !=, svcmpne_) \
    TYPES_FLOAT_BINARY_RET_CMP (gt, >, svcmpgt_) \
    TYPES_FLOAT_BINARY_RET_CMP (lt, <, svcmplt_) \
    TYPES_FLOAT_BINARY_RET_CMP (ge, >=, svcmpge_) \
    TYPES_FLOAT_BINARY_RET_CMP (le, <=, svcmple_) \

#ifdef __cplusplus
#define CPP_TYPES \
    TYPES_BINARY_RET_COND (and_and, &&, svand_) \
    TYPES_BINARY_RET_COND (or_or, ||, svorr_) \
    TYPES_UNARY_RET_COND (lnot, !, svnot_z_) \
    TYPES_TERNARY ()
#endif

/* Declare and define functions.  */
C_TYPES

#ifdef __cplusplus
CPP_TYPES
#endif

#undef DECL_FUNC_UNARY
#define DECL_FUNC_UNARY(type, name, op, intr, su, sz, id) \
  checkfunc_ ## name ## type ## _unary ();

#undef DECL_FUNC_UNARY_FLOAT
#define DECL_FUNC_UNARY_FLOAT(type, name, op, intr, su, sz, id) \
  checkfunc_ ## name ## type ## _unary ();

#undef DECL_FUNC_UNARY_RET_FLOAT
#define DECL_FUNC_UNARY_RET_FLOAT(type, name, op, intr, su, sz, id) \
  checkfunc_ ## name ## type ## _unary ();

#undef DECL_FUNC_INDEX
#define DECL_FUNC_INDEX(rtype, type, intr, su, sz) \
  checkfunc_ ## rtype ## type ## _vindex (); \
  checkfunc_ ## rtype ## type ## _cindex ();

#undef DECL_FUNC_INDEX_FLOAT
#define DECL_FUNC_INDEX_FLOAT(rtype, type, intr, su, sz) \
  checkfunc_ ## rtype ## type ## _vindex (); \
  checkfunc_ ## rtype ## type ## _cindex ();

#undef DECL_FUNC_BINARY
#define DECL_FUNC_BINARY(type, name, op, intr, su, sz) \
  checkfunc_ ## name ## type ## _binary ();

#undef DECL_FUNC_BINARY_SHIFT
#define DECL_FUNC_BINARY_SHIFT(type, name, op, intr, su, sz) \
  checkfunc_ ## name ## type ## _binary ();

#undef DECL_FUNC_BINARY_FLOAT
#define DECL_FUNC_BINARY_FLOAT(type, name, op, intr, su, sz) \
  checkfunc_ ## name ## type ## _binary ();

#undef DECL_FUNC_BINARY_CMP
#define DECL_FUNC_BINARY_CMP(type, name, op, intr, su, sz)  \
  checkfunc_ ## name ## type ## _binary_cmp ();

#undef DECL_FUNC_TERNARY
#define DECL_FUNC_TERNARY(type, su, sz) \
  checkfunc_ ## type ## _ternary ();

#undef DECL_FUNC_INIT
#define DECL_FUNC_INIT(type, su, sz, nelem) \
  checkfunc_ ## type ## _init ();

#undef DECL_FUNC_TERNARY_FLOAT
#define DECL_FUNC_TERNARY_FLOAT(type, su, sz) \
  checkfunc_ ## type ## _ternary ();

#undef DECL_FUNC_BINARY_SCALAR
#define DECL_FUNC_BINARY_SCALAR(scalar, type, name, op, intr, su, sz) \
  checkfunc_ ## name  ## type ## _binary_cscalar (); \
  checkfunc_ ## name  ## type ## _binary_vscalar ();

#undef DECL_FUNC_BINARY_RET_FLOAT_CMP
#define DECL_FUNC_BINARY_RET_FLOAT_CMP(rtype, type, name, op, intr, su, sz) \
  checkfunc_ ## name  ## rtype ## type ## _binary_cmp ();

#undef DECL_FUNC_BINARY_RET_COND
#define DECL_FUNC_BINARY_RET_COND(rtype, type, name, op, intr, su, sz)  \
  checkfunc_ ## name ## rtype ## type ## _binary_cond ();

#undef DECL_FUNC_UNARY_RET_COND
#define DECL_FUNC_UNARY_RET_COND(rtype, type, name, op, intr, su, sz)  \
  checkfunc_ ## name ## rtype ## type ## _unary_cond ();

int
main (void)
{
  C_TYPES

#ifdef __cplusplus
  CPP_TYPES
#endif

  return 0;
}
