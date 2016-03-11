/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+fp" } */

#pragma GCC push_options
#pragma GCC target "arch=armv8-a+nofp"
static void
fn1 ()
{
}
#pragma GCC pop_options
float
fn2 (float a)
{
  return a + 2.0;
}

/* { dg-final { scan-assembler-not "__addsf3" } } */
