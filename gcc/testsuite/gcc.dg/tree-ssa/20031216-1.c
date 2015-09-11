/* { dg-do compile } */ 
/* { dg-options "-O1 -fdump-tree-optimized" } */

extern void link_error (void);

void
foo (int b)
{
  int a;
  a = b + 2;
  a--;
  a--;
  if (a != b)
    link_error ();
}

/* The comparison should be eliminated, there should be no reference
   to link_error.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
