/* PR48985 */
/* { dg-do run } */
/* { dg-options "-std=gnu89" } */
/* { dg-skip-if "packed attribute missing for struct s" { "epiphany-*-*" } } */

#define __builtin_object_size __builtin_dynamic_object_size
#include "builtin-object-size-11.c"
