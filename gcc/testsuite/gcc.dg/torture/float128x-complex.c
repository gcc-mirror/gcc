/* Test _Float128x complex arithmetic.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float128x } */
/* { dg-require-effective-target float128x_runtime } */

#define WIDTH 128
#define EXT 1
#include "floatn-complex.h"
