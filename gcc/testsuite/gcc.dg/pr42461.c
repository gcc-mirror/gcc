/* PR rtl-optimization/42461 */
/* Reported by Patrick Pelissier <patrick.pelissier@gmail.com> */

/* { dg-do link } */
/* { dg-options "-O2" } */

extern int link_failure (int) __attribute__ ((pure));

int main (void)
{
  if (link_failure (0) < 1)
    __builtin_unreachable ();
  return 0;
}
