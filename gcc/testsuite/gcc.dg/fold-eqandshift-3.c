/* PR middle-end/29726 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

void foo (void);

int test1 (int a)
{
  if ((a >> 3) & 134217728)
    foo ();
}

int test2 (unsigned int b)
{
  if ((b >> 3) & 134217728)
    foo ();
}

/* { dg-final { scan-tree-dump-times "\\(a \& 1073741824\\) != 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\\(b \& 1073741824\\) != 0" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
