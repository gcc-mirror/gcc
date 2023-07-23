/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */


void f(signed short *a, unsigned char *c)
{
  unsigned long b = *a;
  *c = ((unsigned char)b);
}


/* { dg-final { scan-tree-dump-not "\\(long unsigned int\\)" "optimized"} } */
