/* PR rtl-optimization/83986 */
/* { dg-do compile } */
/* { dg-options "-g -O2 -fsched2-use-superblocks -funwind-tables --param max-pending-list-length=1" } */

int v;

int
foo (int x)
{
  v &= !!v && !!x;
  if (v != 0)
    foo (0);
  return 0;
}
