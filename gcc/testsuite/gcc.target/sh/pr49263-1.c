/* Verify that TST #imm, R0 instruction is generated when QImode or HImode
   values are tested against a negative constant.  */
/* { dg-do compile }  */
/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler-not "and" } }  */
/* { dg-final { scan-assembler-not "bclr" } }  */
/* { dg-final { scan-assembler-times "extu" 1 } }  */
/* { dg-final { scan-assembler-times "exts" 1 } }  */

#define make_func(__valtype__, __valget__, __tstval__, __suff__)\
  int test_imm_##__tstval__##__suff__ (__valtype__ val) \
    {\
      return ((__valget__) & (- 0x##__tstval__  << 0)) ? -20 : -40;\
    }

#define make_func_0_F(__valtype__, __valget__, __y__, __suff__)\
  make_func (__valtype__, __valget__, __y__##0, __suff__)\
  make_func (__valtype__, __valget__, __y__##1, __suff__)\
  make_func (__valtype__, __valget__, __y__##2, __suff__)\
  make_func (__valtype__, __valget__, __y__##3, __suff__)\
  make_func (__valtype__, __valget__, __y__##4, __suff__)\
  make_func (__valtype__, __valget__, __y__##5, __suff__)\
  make_func (__valtype__, __valget__, __y__##6, __suff__)\
  make_func (__valtype__, __valget__, __y__##7, __suff__)\
  make_func (__valtype__, __valget__, __y__##8, __suff__)\
  make_func (__valtype__, __valget__, __y__##9, __suff__)\
  make_func (__valtype__, __valget__, __y__##A, __suff__)\
  make_func (__valtype__, __valget__, __y__##B, __suff__)\
  make_func (__valtype__, __valget__, __y__##C, __suff__)\
  make_func (__valtype__, __valget__, __y__##D, __suff__)\
  make_func (__valtype__, __valget__, __y__##E, __suff__)\
  make_func (__valtype__, __valget__, __y__##F, __suff__)\

#define make_funcs_0_FF(__valtype__, __valget__, __suff__)\
  make_func_0_F (__valtype__, __valget__, 0, __suff__)\
  make_func_0_F (__valtype__, __valget__, 1, __suff__)\
  make_func_0_F (__valtype__, __valget__, 2, __suff__)\
  make_func_0_F (__valtype__, __valget__, 3, __suff__)\
  make_func_0_F (__valtype__, __valget__, 4, __suff__)\
  make_func_0_F (__valtype__, __valget__, 5, __suff__)\
  make_func_0_F (__valtype__, __valget__, 6, __suff__)\
  make_func_0_F (__valtype__, __valget__, 7, __suff__)\
  make_func_0_F (__valtype__, __valget__, 8, __suff__)\
  make_func_0_F (__valtype__, __valget__, 9, __suff__)\
  make_func_0_F (__valtype__, __valget__, A, __suff__)\
  make_func_0_F (__valtype__, __valget__, B, __suff__)\
  make_func_0_F (__valtype__, __valget__, C, __suff__)\
  make_func_0_F (__valtype__, __valget__, D, __suff__)\
  make_func_0_F (__valtype__, __valget__, E, __suff__)\
  make_func_0_F (__valtype__, __valget__, F, __suff__)\

make_funcs_0_FF (signed char*, *val, int8_mem)
make_funcs_0_FF (signed char, val, int8_reg)

make_funcs_0_FF (unsigned char*, *val, uint8_mem)
make_funcs_0_FF (unsigned char, val, uint8_reg)

make_funcs_0_FF (short*, *val, int16_mem)
make_funcs_0_FF (short, val, int16_reg)

make_funcs_0_FF (unsigned short*, *val, uint16_mem)
make_funcs_0_FF (unsigned short, val, uint16_reg)
