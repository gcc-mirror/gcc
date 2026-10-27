/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <arm_sve.h>

/* PR target/127771.  */

void
g (svbool_t p, svuint32_t a)
{
  svmul_x (p, a, svdup_u32 (1));
}

/* The placeholder for a discarded result cannot be an SSA name when the
   return type is an aggregate.  */

void
h (svuint32_t a, svuint32_t b)
{
  svcreate2_u32 (a, b);
}
