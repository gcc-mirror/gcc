/* { dg-do compile } */
/* { dg-options "-march=rv64imfd_zfa_zfh -mabi=lp64d"  { target { rv64 } } } */
/* { dg-options "-march=rv32imfd_zfa_zfh -mabi=ilp32d" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Og" "-Oz"} } */

/* Canonical NaN is, positive, quiet NaN with zero payload.  */

#define TYPE_h _Float16
#define TYPE_s float
#define TYPE_d double

#define DECL_TYPE(TYPE_SHORT) TYPE_##TYPE_SHORT

#define DECL_FUNC(TYPE_SHORT, N, VALUE)       \
  DECL_TYPE (TYPE_SHORT) const_##TYPE_SHORT##_##N (void)       \
    {       \
      return VALUE;       \
    }

/* Canonical NaN.  */
DECL_FUNC (h, 1, __builtin_nanf16 (""))
DECL_FUNC (s, 1, __builtin_nanf (""))
DECL_FUNC (d, 1, __builtin_nan (""))
DECL_FUNC (h, 2, __builtin_nanf16 ("0"))
DECL_FUNC (s, 2, __builtin_nanf ("0"))
DECL_FUNC (d, 2, __builtin_nan ("0"))

/* { dg-final { scan-assembler-times "fli\\.h\tfa\[0-9\],nan\n" 2 } } */
/* { dg-final { scan-assembler-times "fli\\.s\tfa\[0-9\],nan\n" 2 } } */
/* { dg-final { scan-assembler-times "fli\\.d\tfa\[0-9\],nan\n" 2 } } */
