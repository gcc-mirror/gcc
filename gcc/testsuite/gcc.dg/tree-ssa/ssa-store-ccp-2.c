/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

const int conststaticvariable;

int f(void)
{
  return conststaticvariable;
}

/* There should be a reference to conststaticvariable since it may
   may be overriden at link time.  */
/* { dg-final { scan-tree-dump-times "conststaticvariable" 1 "optimized" { xfail { *-*-mingw* *-*-cygwin* } } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
