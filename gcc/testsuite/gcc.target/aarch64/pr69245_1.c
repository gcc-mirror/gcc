/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+fp -fomit-frame-pointer" } */

#pragma GCC target "arch=armv8-a+nofp"
long a;
static void
fn1 ()
{
}

#pragma GCC target "arch=armv8-a+fp"
float
fn2 (float a)
{
  return a + 2.0;
}

/* { dg-final { scan-assembler-not "__addsf3" } } */
