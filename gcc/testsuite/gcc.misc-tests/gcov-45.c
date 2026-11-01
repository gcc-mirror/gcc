/* PR gcov-profile/128346 */
/* { dg-do compile } */
/* { dg-options "-O1 -g -fno-thread-jumps -fprofile-arcs -fdump-tree-cfg-details" } */

int bar (int);

#pragma GCC suppress_coverage begin
int
foo (int a)
{
  a;
#pragma GCC suppress_coverage end
  if (a > 3)
    a = bar (a);
  return bar (a);
}

/* A debug marker must not hide the coverage-suppression nop.  */
/* { dg-final { scan-tree-dump-not "Merging blocks 2 and 3" "cfg" } } */
