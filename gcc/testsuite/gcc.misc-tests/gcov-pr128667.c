/* PR gcov-profile/128667 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -fprofile-arcs -fdump-tree-cfg" } */

int g, h;

void
f (int x)
{
  if (x)
    {
#pragma GCC suppress_coverage begin
      g = 1;
#pragma GCC suppress_coverage end
      ;
#pragma GCC suppress_coverage begin
      h = 2;
#pragma GCC suppress_coverage end
    }
  g++;
}

/* The marker for the empty statement must not create an unsuppressed
   block between the two suppressed assignments.  There are three body
   blocks, plus the entry and exit nodes.  */
/* { dg-final { scan-tree-dump "nodes: 0 1 2 3 4\n" "cfg" } } */
