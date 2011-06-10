/* { dg-do compile } */
/* { dg-skip-if "incompatible options" { arm*-*-* } { "-march=*" } { "-march=armv6" "-march=armv6j" "-march=armv6z" } } */
/* { dg-options "-mcpu=arm1136jf-s -mthumb -O2" } */

void f(unsigned a, unsigned b, unsigned c, unsigned d)
{
  if (a <= b || c > d)
    foo();
  else
    bar();
}

/* { dg-final { scan-assembler-not "uxtb" } } */
