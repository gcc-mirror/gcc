/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */


void f(signed char *a, unsigned char *c)
{
  unsigned short b = *a;
  *c = ((unsigned char)b);
}


/* { dg-final { scan-tree-dump-not "\\(short unsigned int\\)" "optimized"} } */
