/* PR target/109276 */
/* { dg-do compile } */
/* { dg-options "-march=x86-64" } */
/* { dg-additional-options "-mpreferred-stack-boundary=2" { target ia32 } } */

long long a;
long double b;

void
foo (void)
{
  b += a;
}
