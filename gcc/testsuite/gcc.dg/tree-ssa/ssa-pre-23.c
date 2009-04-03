/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

struct { int x; int y; } global;
void foo(int n)
{
  int i;
  for ( i=0; i<n; i++)
    global.y += global.x*global.x;
}

/* { dg-final { scan-tree-dump "Eliminated: 2" "pre" } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
