/* PR tree-optimization/35468 */
/* { dg-require-effective-target no_const_addr_space } */

void
foo (void)
{
  *(char *) "c" = 'x';
}
