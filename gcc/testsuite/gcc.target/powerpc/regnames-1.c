/* { dg-do compile { target powerpc*-*-linux* } } */
/* { dg-options "-mregnames" } */

register double f17 asm ("f17");
double foo (void)
{
  return f17;
}
