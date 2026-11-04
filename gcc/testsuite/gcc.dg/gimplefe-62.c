/* { dg-do compile } */
/* { dg-options "-O -fgimple -fdump-tree-ssa -Wno-psabi" } */

/* The calls are dead, so no target needs to expand them.  */

typedef unsigned int v4usi __attribute__ ((vector_size (16)));

void __GIMPLE
f (unsigned int x, v4usi y)
{
  int r1;
  int r2;
  v4usi r3;
  int r4;
  int r5;

  r1 = .CLZ (x, 32);
  r2 = .CTZ (x);
  r3 = .CTZ (y);
  r4 = .POPCOUNT (x);
  r5 = .POPCOUNT (x, 0);
  return;
}

/* { dg-final { scan-tree-dump-times {\.CLZ \(} 1 "ssa" } } */
/* { dg-final { scan-tree-dump-times {\.CTZ \(} 2 "ssa" } } */
/* { dg-final { scan-tree-dump-times {\.POPCOUNT \(} 2 "ssa" } } */
