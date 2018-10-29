/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-phiopt2-details" } */
int f(int a, int b)
{
   int c = b;
   if (a != b)
    c = a;
   return c;
}

/* Should have no ifs left after straightening.  */
/* { dg-final { scan-tree-dump-times "if " 0 "phiopt2"} } */
