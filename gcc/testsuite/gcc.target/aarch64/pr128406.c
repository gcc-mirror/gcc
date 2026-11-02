/* PR tree-optimization/128406 */
/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <arm_neon.h>

extern int rt (void) __attribute__ ((returns_twice));
extern void bar (void);
extern void use (int32_t);

/* y occurs in abnormal PHIs.  The NEON folds give forwprop a CONSTRUCTOR
   of the high half of x and of the splat of y.  A new splat of y at the
   CONSTRUCTOR would be a use of y after its redefinition.  */

void
f (int32x4_t *out, int32x4_t x, int32_t y)
{
  int32x2_t w = vdup_n_s32 (y);
  bar ();
  y = y * 3;
  *out = vcombine_s32 (vget_high_s32 (x), w);
  bar ();
  rt ();
  use (y);
}
