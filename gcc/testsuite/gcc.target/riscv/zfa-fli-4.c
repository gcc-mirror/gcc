/* { dg-do compile } */
/* { dg-options "-march=rv64imfd_zfa_zfh -mabi=lp64d"  { target { rv64 } } } */
/* { dg-options "-march=rv32imfd_zfa_zfh -mabi=ilp32d" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Og" "-Oz"} } */

#define TYPE_h _Float16
#define TYPE_s float
#define TYPE_d double

#define DECL_TYPE(TYPE_SHORT) TYPE_##TYPE_SHORT

#define DECL_FUNC(TYPE_SHORT, N, VALUE)       \
  DECL_TYPE (TYPE_SHORT) const_##TYPE_SHORT##_##N (void)       \
    {       \
      return VALUE;       \
    }

#define DECL_FINITE_FUNCS(TYPE_SHORT)       \
  DECL_FUNC (TYPE_SHORT, 00, -1)       \
  DECL_FUNC (TYPE_SHORT, 02, 0.0000152587890625)       \
  DECL_FUNC (TYPE_SHORT, 03, 0.000030517578125)       \
  DECL_FUNC (TYPE_SHORT, 04, 0.00390625)       \
  DECL_FUNC (TYPE_SHORT, 05, 0.0078125)       \
  DECL_FUNC (TYPE_SHORT, 06, 0.0625)       \
  DECL_FUNC (TYPE_SHORT, 07, 0.125)       \
  DECL_FUNC (TYPE_SHORT, 08, 0.25)       \
  DECL_FUNC (TYPE_SHORT, 09, 0.3125)       \
  DECL_FUNC (TYPE_SHORT, 10, 0.375)       \
  DECL_FUNC (TYPE_SHORT, 11, 0.4375)       \
  DECL_FUNC (TYPE_SHORT, 12, 0.5)       \
  DECL_FUNC (TYPE_SHORT, 13, 0.625)       \
  DECL_FUNC (TYPE_SHORT, 14, 0.75)       \
  DECL_FUNC (TYPE_SHORT, 15, 0.875)       \
  DECL_FUNC (TYPE_SHORT, 16, 1)       \
  DECL_FUNC (TYPE_SHORT, 17, 1.25)       \
  DECL_FUNC (TYPE_SHORT, 18, 1.5)       \
  DECL_FUNC (TYPE_SHORT, 19, 1.75)       \
  DECL_FUNC (TYPE_SHORT, 20, 2)       \
  DECL_FUNC (TYPE_SHORT, 21, 2.5)       \
  DECL_FUNC (TYPE_SHORT, 22, 3)       \
  DECL_FUNC (TYPE_SHORT, 23, 4)       \
  DECL_FUNC (TYPE_SHORT, 24, 8)       \
  DECL_FUNC (TYPE_SHORT, 25, 16)       \
  DECL_FUNC (TYPE_SHORT, 26, 128)       \
  DECL_FUNC (TYPE_SHORT, 27, 256)       \
  DECL_FUNC (TYPE_SHORT, 28, 32768)       \
  DECL_FUNC (TYPE_SHORT, 29, 65536)

/* Finite numbers (except 2^16 in _Float16, making an inf).  */
DECL_FINITE_FUNCS (h)
DECL_FINITE_FUNCS (s)
DECL_FINITE_FUNCS (d)

/* min.  */
DECL_FUNC (h, 01, __FLT16_MIN__)
DECL_FUNC (s, 01, __FLT_MIN__)
DECL_FUNC (d, 01, __DBL_MIN__)

/* inf.  */
DECL_FUNC (h, 30, __builtin_inff16 ())
DECL_FUNC (s, 30, __builtin_inff ())
DECL_FUNC (d, 30, __builtin_inf ())

/* nan.  */
DECL_FUNC (h, 31, __builtin_nanf16 (""))
DECL_FUNC (s, 31, __builtin_nanf (""))
DECL_FUNC (d, 31, __builtin_nan (""))


/* { dg-final { scan-assembler-times "fli\\.h" 32 } } */
/* { dg-final { scan-assembler-times "fli\\.s"  32 } } */
/* { dg-final { scan-assembler-times "fli\\.d"  32 } } */
