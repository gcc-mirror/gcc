/* PR rtl-optimization/105211 */
/* { dg-do compile } */
/* { dg-options "-Os -ffast-math" } */
/* { dg-add-options float32 } */
/* { dg-require-effective-target float32 } */

short
foo (_Float32 f)
{
  return __builtin_roundf (f);
}
