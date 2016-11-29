/* Test _Float16 type-generic built-in functions: __builtin_isinf_sign.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float16 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float16_runtime } */

#define WIDTH 16
#define EXT 0
#include "floatn-tg-2.h"
