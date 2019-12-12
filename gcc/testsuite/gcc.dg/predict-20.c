/* PR tree-optimization/86925 */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

int a, b;

void
c ()
{
  for (;;)
    {
      if (__builtin_expect (b < 0, 0))
	break;
      if (__builtin_expect (a, 1))
	break;
    }
  int d = b < 0;
  if (__builtin_expect (d, 0))
    asm("");
}

/* { dg-final { scan-tree-dump-times "__builtin_expect heuristics of edge" 3 "profile_estimate"} } */
