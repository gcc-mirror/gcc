/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom1" } */
  
void bar1 (void);
void bar2 (void);

void
foo (unsigned int a, unsigned int b)
{
  if (a >= b)
    bar1 ();
  else if (a <= b)
    bar2 ();
}

/* The second conditional is redundant since we know it must be
   true (to reach the second condition we know a < b via the first
   conditional.  */

/* { dg-final { scan-tree-dump-times "if " 1 "dom1" } } */

