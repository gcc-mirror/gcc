/* Test _Float16 alignment.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16 } */

#define WIDTH 16
#define EXT 0
#include "floatn-align.h"
