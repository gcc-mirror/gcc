/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

/* Testcase extracted from PR15353.  */

extern void bar(void);

void foo (int x, int a)
{
  /* if ((x < a) || (x != a)) return; else bar (); */
  if (x < a)
    return;
  if (x != a)
    return;

  /* else */
  bar ();
}

/* { dg-final { scan-tree-dump "!=" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
