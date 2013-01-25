/* PR target/53187 */
/* { dg-do compile } */
/* { dg-skip-if "no support for hard-float VFP ABI" { arm_thumb1 } { "-march=*" } { "" } } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-options "-march=armv7-a -mfloat-abi=hard -O2" } */

void bar (int);

void
foo (int x, double y, double z)
{
  _Bool t = z >= y;
  if (!t || x)
    bar (t ? 1 : 16);
}
