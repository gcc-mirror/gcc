/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-lim2-details" } */

struct { int x; int y; } global;
void foo(int n)
{
  int i;
  for ( i=0; i<n; i++)
    global.y += global.x*global.x;
}

/* { dg-final { scan-tree-dump "Executing store motion of global.y" "lim2" } } */
/* { dg-final { scan-tree-dump "Moving statement.*global.x.*out of loop 1" "lim2" } } */
