/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom1 -fdump-tree-dom2" } */
  
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


/* We do not canonicalize the second conditional immediately after going
   into SSA form, thus the first dominator pass is unable to remove
   the useless conditional.   Thus the xfailed test. 

   However, the second conditional is canonicalized before the second
   dominator optimizer pass and we do want to verify the call to 
   bar2 was eliminated.  */
/* { dg-final { scan-tree-dump-times "bar2" 0 "dom1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "bar2" 0 "dom2" } } */

