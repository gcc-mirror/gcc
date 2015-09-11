/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1" } */
void bar (void);

void
foo (unsigned int a)
{
  if ((a >> 5) & 1)
    bar ();
}



/* There should be no casts to a _Bool since we can use the temporary
   holding (a>>5)&1 directly.  */
/* { dg-final { scan-tree-dump-times "\\(_Bool\\)" 0 "forwprop1"} } */
