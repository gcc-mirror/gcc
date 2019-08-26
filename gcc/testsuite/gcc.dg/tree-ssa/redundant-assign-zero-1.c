/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse-details" } */

void blah (char *);

void bar ()
{
  char a[256] = "";
  a[3] = 0; 
  blah (a);
}

/* { dg-final { scan-tree-dump-times "Deleted redundant store" 1 "dse1"} } */
