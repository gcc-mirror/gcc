/* { dg-do compile } */
/* { dg-options "-O -fno-tree-dce -fno-tree-dse -fchecking" } */

int i;
void
foo (void)
{
  i &= i && __builtin_bswap16 (i);
}
