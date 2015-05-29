/* { dg-do compile } */
/* { dg-options "-O2 -march=armv7-a -mfloat-abi=softfp -mfpu=neon -fno-unsafe-math-optimizations -fdump-rtl-combine" { target { arm*-*-* } } } */
/* { dg-options "-O2 -fno-unsafe-math-optimizations -fdump-rtl-combine" { target { ! arm*-*-* } } } */


static const double one=1.0;

double
f(double x)
{
  return x*(one+x);
}

/* { dg-final { scan-rtl-dump-not "\\(plus:DF \\(mult:DF" "combine" } } */
