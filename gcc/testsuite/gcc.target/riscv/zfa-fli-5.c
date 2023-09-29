/* { dg-do compile } */
/* { dg-options "-march=rv64imf_zfa_zvfh -mabi=lp64f"  { target { rv64 } } } */
/* { dg-options "-march=rv32imf_zfa_zvfh -mabi=ilp32f" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Og" "-Oz"} } */

/* Even if 'Zfh' is disabled, "fli.h" is usable when
   both 'Zfa' and 'Zvfh' are available.  */
#ifdef __riscv_zfh
#error Invalid feature macro defined
#endif

#define TYPE_h _Float16

#define DECL_TYPE(TYPE_SHORT) TYPE_##TYPE_SHORT

#define DECL_FUNC(TYPE_SHORT, N, VALUE)                                       \
  DECL_TYPE (TYPE_SHORT) const_##TYPE_SHORT##_##N (void)                      \
    {                                                                         \
      return VALUE;                                                           \
    }

#define DECL_FINITE_FUNCS(TYPE_SHORT)                                         \
  DECL_FUNC (TYPE_SHORT, 00, -1)                                              \
  DECL_FUNC (TYPE_SHORT, 02, 0.0000152587890625)                              \
  DECL_FUNC (TYPE_SHORT, 03, 0.000030517578125)                               \
  DECL_FUNC (TYPE_SHORT, 04, 0.00390625)                                      \
  DECL_FUNC (TYPE_SHORT, 05, 0.0078125)                                       \
  DECL_FUNC (TYPE_SHORT, 06, 0.0625)                                          \
  DECL_FUNC (TYPE_SHORT, 07, 0.125)                                           \
  DECL_FUNC (TYPE_SHORT, 08, 0.25)                                            \
  DECL_FUNC (TYPE_SHORT, 09, 0.3125)                                          \
  DECL_FUNC (TYPE_SHORT, 10, 0.375)                                           \
  DECL_FUNC (TYPE_SHORT, 11, 0.4375)                                          \
  DECL_FUNC (TYPE_SHORT, 12, 0.5)                                             \
  DECL_FUNC (TYPE_SHORT, 13, 0.625)                                           \
  DECL_FUNC (TYPE_SHORT, 14, 0.75)                                            \
  DECL_FUNC (TYPE_SHORT, 15, 0.875)                                           \
  DECL_FUNC (TYPE_SHORT, 16, 1)                                               \
  DECL_FUNC (TYPE_SHORT, 17, 1.25)                                            \
  DECL_FUNC (TYPE_SHORT, 18, 1.5)                                             \
  DECL_FUNC (TYPE_SHORT, 19, 1.75)                                            \
  DECL_FUNC (TYPE_SHORT, 20, 2)                                               \
  DECL_FUNC (TYPE_SHORT, 21, 2.5)                                             \
  DECL_FUNC (TYPE_SHORT, 22, 3)                                               \
  DECL_FUNC (TYPE_SHORT, 23, 4)                                               \
  DECL_FUNC (TYPE_SHORT, 24, 8)                                               \
  DECL_FUNC (TYPE_SHORT, 25, 16)                                              \
  DECL_FUNC (TYPE_SHORT, 26, 128)                                             \
  DECL_FUNC (TYPE_SHORT, 27, 256)                                             \
  DECL_FUNC (TYPE_SHORT, 28, 32768)                                           \
  DECL_FUNC (TYPE_SHORT, 29, 65536)

/* Finite numbers (except 2^16 in _Float16, making an inf).  */
DECL_FINITE_FUNCS (h)

/* min.  */
DECL_FUNC (h, 01, __FLT16_MIN__)

/* inf.  */
DECL_FUNC (h, 30, __builtin_inff16 ())


/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],-1.0\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],1.52587890625e-05\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],3.0517578125e-05\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],0.00390625\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],0.0078125\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],0.0625\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],0.125\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],0.25\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],0.3125\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],0.375\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],0.4375\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],0.5\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],0.625\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],0.75\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],0.875\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],1.0\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],1.25\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],1.5\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],1.75\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],2.0\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],2.5\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],3.0\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],4.0\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],8.0\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],16.0\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],128.0\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],256.0\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],32768.0\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],inf\n" 2 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],min\n" 1 } } */
/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],nan\n" 1 } } */


/* nan.  */
DECL_FUNC (h, 31, __builtin_nanf16 (""))

/* { dg-final { scan-assembler-times "fli\.h\tfa\[0-9\],nan\n"           1 } } */
