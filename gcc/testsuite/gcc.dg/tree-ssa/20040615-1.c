/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom1" } */
  
void bar1 (void);
void bar2 (void);

void
foo (unsigned int a, unsigned int b)
{
  if (a >= b)
    bar1 ();
  else if (b <= a)
    bar2 ();
}

/* { dg-final { scan-tree-dump-times "bar2" 0 "dom1" } } */
