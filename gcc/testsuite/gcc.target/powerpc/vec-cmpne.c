/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

/* Test that the vec_cmpne builtin generates the expected Altivec
   instructions.  */

#include "vec-cmpne.h"

define_test_functions (int, signed int, signed int, si);
define_test_functions (int, unsigned int, unsigned int, ui);
define_test_functions (short, signed short, signed short, ss);
define_test_functions (short, unsigned short, unsigned short, us);
define_test_functions (char, signed char, signed char, sc);
define_test_functions (char, unsigned char, unsigned char, uc);
define_test_functions (int, signed int, float, ff);

/* { dg-final { scan-assembler-times {\mvcmpequb\M}  2 } } */
/* { dg-final { scan-assembler-times {\mvcmpequh\M}  2 } } */
/* { dg-final { scan-assembler-times {\mvcmpequw\M}  2 } } */
