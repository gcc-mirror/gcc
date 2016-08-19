/* Test _Float128 type-generic built-in functions.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float128 } */
/* { dg-require-effective-target float128_runtime } */

#define WIDTH 128
#define EXT 0
#include "floatn-tg.h"
