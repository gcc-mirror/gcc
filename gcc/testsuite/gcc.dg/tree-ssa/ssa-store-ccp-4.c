/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-skip-if "" { *-*-darwin* *-*-mingw* } { "*" } { "" } } */
/* { dg-options "-O2 -fno-common -fpic -fdump-tree-optimized" } */

const int conststaticvariable;

int f(void)
{
  return conststaticvariable;
}

/* There should be a reference to conststaticvariable since it may
   may be overriden at run time.  */
/* { dg-final { scan-tree-dump-times "conststaticvariable" 1 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
