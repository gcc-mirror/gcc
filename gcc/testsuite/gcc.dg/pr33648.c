/* PR rtl-optimization/33648 */
/* { dg-do compile } */
/* { dg-require-effective-target freorder } */
/* { dg-options "-O2 -fmodulo-sched -freorder-blocks-and-partition" } */

unsigned res;

void
foo (unsigned code, int len)
{
  int i;
  for (i = 0; i < len; i++)
    res |= code & 1;
}
