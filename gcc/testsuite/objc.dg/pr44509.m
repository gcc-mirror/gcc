/* PR bootstrap/44509 */
/* { dg-do compile } */
/* { dg-options "--param ggc-min-expand=0 --param ggc-min-heapsize=0" } */

double
foo (void)
{
  return __DBL_MAX__ - __FLT_MAX__;
}
