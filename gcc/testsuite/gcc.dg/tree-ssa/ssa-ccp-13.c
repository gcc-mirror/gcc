/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

static const char f[3] = "?";

int foo()
{
  int i = 0;
  return f[i] != '?';
}

/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */

