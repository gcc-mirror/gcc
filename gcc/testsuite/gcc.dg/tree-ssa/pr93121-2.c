/* PR libstdc++/93121 */
/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

union U { int a[3]; struct S { int d; int a : 3; int b : 24; int c : 5; int e; } b; };
const union U u = { .a = { 0x7efa3412, 0x5a876543, 0x1eeffeed } };
int a, b, c;

void
foo ()
{
  a = u.b.a;
  b = u.b.b;
  c = u.b.c;
}

/* { dg-final { scan-tree-dump-times "a = 3;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "b = 5303464;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "c = 11;" 1 "optimized" { target le } } } */
/* { dg-final { scan-tree-dump-times "a = 2;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "b = -2868438;" 1 "optimized" { target be } } } */
/* { dg-final { scan-tree-dump-times "c = 3;" 1 "optimized" { target be } } } */
