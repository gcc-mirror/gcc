/* PR 55113 */
/* { dg-lto-do link } */
/* { dg-lto-options { { -flto -fshort-double -O0 } } }*/
/* { dg-skip-if "PR60410" { x86_64-*-* || { i?86-*-* && lp64 } } } */
/* { dg-skip-if "PR60410" { i?86-*-solaris2.1[0-9]* } } */

int 
main(void)
{
  float a = 1.0;
  float b = 2.0;
  double f = a + b * 1e-12;
  return (int)f - 1;
}
