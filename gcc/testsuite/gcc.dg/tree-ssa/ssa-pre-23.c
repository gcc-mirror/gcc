/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-loop-im -fdump-tree-pre-stats" } */

struct { int x; int y; } global;
void foo(int n)
{
  int i;
  for ( i=0; i<n; i++)
    global.y += global.x*global.x;
}

/* { dg-final { scan-tree-dump "Eliminated: 3" "pre" } } */
