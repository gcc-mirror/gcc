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

/* Non-canonical NaN.  */
DECL_FUNC (h, 1, __builtin_nansf16 (""))
DECL_FUNC (s, 1, __builtin_nansf (""))
DECL_FUNC (d, 1, __builtin_nans (""))
DECL_FUNC (h, 2, __builtin_nansf16 ("0"))
DECL_FUNC (s, 2, __builtin_nansf ("0"))
DECL_FUNC (d, 2, __builtin_nans ("0"))
DECL_FUNC (h, 3, __builtin_nanf16 ("1"))
DECL_FUNC (s, 3, __builtin_nanf ("1"))
DECL_FUNC (d, 3, __builtin_nan ("1"))
DECL_FUNC (h, 4, __builtin_nansf16 ("1"))
DECL_FUNC (s, 4, __builtin_nansf ("1"))
DECL_FUNC (d, 4, __builtin_nans ("1"))

/* Canonical NaN, negated (making it non-canonical).  */
DECL_FUNC (h, 5, -__builtin_nanf16 (""))
DECL_FUNC (s, 5, -__builtin_nanf (""))
DECL_FUNC (d, 5, -__builtin_nan (""))

/* { dg-final { scan-assembler-not "fli\\.\[hsd]\tfa\[0-9\],nan\n" } } */
