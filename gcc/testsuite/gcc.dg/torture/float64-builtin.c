/* Test _Float64 built-in functions.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float64 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float64_runtime } */
/* { dg-require-effective-target double64plus } */

#define WIDTH 64
#define EXT 0
#include "floatn-builtin.h"
