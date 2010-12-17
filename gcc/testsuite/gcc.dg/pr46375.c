/* PR debug/46375 */
/* Reported by Zdenek Sojka <zsojka@seznam.cz> */
/* { dg-do compile } */
/* { dg-options "-fgcse -fno-tree-dominator-opts -fcompare-debug -O" } */

void bar (void);

void
foo (int **pp)
{
  int *p = 0;
  if (pp)
    p = *pp;
  if (p && *p)
    bar ();
}
