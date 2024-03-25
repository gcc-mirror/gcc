/* PR tree-optimization/111967 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-forwprop -fdump-tree-evrp-all" } */

void bar (char *);
int a;
char *b;

void
foo (void)
{
  long c = a & 3;
  if (c)
    bar (b + c);
}
