/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-optimized" } */
extern int printf (const char *, ...);
int main(int argc, int b)
{
  /* We should be able to get rid of the a - i.  */
  int i;
  for (i = 0; i < 50; i++)
    {
      int a = b + i;
      int c = a - i;
      int d = argc + b;
      printf ("%d %d\n", c,d);
    }
}
/* { dg-final { scan-tree-dump-times "a - i" 0 "optimized"} } */
