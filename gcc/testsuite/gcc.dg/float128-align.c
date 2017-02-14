/* Test _Float128 alignment.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-add-options float128 } */
/* { dg-require-effective-target float128 } */

#define WIDTH 128
#define EXT 0
#include "floatn-align.h"
