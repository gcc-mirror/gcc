/* { dg-do compile } */
/* Skipped on MIPS GNU/Linux and IRIX target because __PIC__ can be
   defined for executables as well as shared libraries.  */
/* { dg-skip-if "" { *-*-darwin* hppa*64*-*-* mips*-*-linux* mips*-*-irix* } { "*" } { "" } } */
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
