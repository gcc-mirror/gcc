/* PR target/79495 */
/* { dg-do compile } */
/* { dg-options "-O2 -msoft-float" } */

long double dnan = 1.0l/0.0l - 1.0l/0.0l;
long double x = 1.0l;
void fn1 (void)
{
  if (dnan != x)
    x = 1.0;
}
