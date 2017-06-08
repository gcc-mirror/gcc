/* Test _Float32 type-generic built-in functions: __builtin_f__builtin_iszero,
   __builtin_issubnormal.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float32 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float32_runtime } */

#define WIDTH 32
#define EXT 0
#include "floatn-tg-4.h"
