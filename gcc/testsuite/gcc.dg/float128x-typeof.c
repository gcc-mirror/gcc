/* Test _Float128x constant types.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-add-options float128x } */
/* { dg-require-effective-target float128x } */

#define WIDTH 128
#define EXT 1
#include "floatn-typeof.h"
