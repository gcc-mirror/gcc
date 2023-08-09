/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec -O2 " } */

/* Test that the vec_cmpne builtin works as expected.  */

#include "vec-cmpne.h"

define_test_functions (int, signed int, signed int, si);
define_test_functions (int, unsigned int, unsigned int, ui);
define_test_functions (short, signed short, signed short, ss);
define_test_functions (short, unsigned short, unsigned short, us);
define_test_functions (char, signed char, signed char, sc);
define_test_functions (char, unsigned char, unsigned char, uc);
define_test_functions (int, signed int, float, ff);

define_init_verify_functions (int, signed int, signed int, si);
define_init_verify_functions (int, unsigned int, unsigned int, ui);
define_init_verify_functions (short, signed short, signed short, ss);
define_init_verify_functions (short, unsigned short, unsigned short, us);
define_init_verify_functions (char, signed char, signed char, sc);
define_init_verify_functions (char, unsigned char, unsigned char, uc);
define_init_verify_functions (int, signed int, float, ff);

int main ()
{
  execute_test_functions (int, signed int, signed int, si);
  execute_test_functions (int, unsigned int, unsigned int, ui);
  execute_test_functions (short, signed short, signed short, ss);
  execute_test_functions (short, unsigned short, unsigned short, us);
  execute_test_functions (char, signed char, signed char, sc);
  execute_test_functions (char, unsigned char, unsigned char, uc);
  execute_test_functions (int, signed int, float, ff);

  return 0;
}
