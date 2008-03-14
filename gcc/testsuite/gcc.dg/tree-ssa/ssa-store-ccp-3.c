/* { dg-do compile } */
/* { dg-skip-if "" { *-*-darwin* } { "*" } { "" } } */
/* { dg-options "-O2 -fno-common -fdump-tree-optimized" } */

const int conststaticvariable;

int f(void)
{
  return conststaticvariable;
}

/* There should be no reference for nonpic targets to
   conststaticvariable as we should have inlined the 0. */
/* { dg-final { scan-tree-dump-times "conststaticvariable" 0 "optimized" { target nonpic } } } */
/* { dg-final { scan-tree-dump-times "conststaticvariable" 1 "optimized" { target { ! nonpic } } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
