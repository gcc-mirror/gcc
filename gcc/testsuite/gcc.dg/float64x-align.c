/* Test _Float64 alignment.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-add-options float64x } */
/* { dg-require-effective-target float64x } */

#define WIDTH 64
#define EXT 1
#include "floatn-align.h"
