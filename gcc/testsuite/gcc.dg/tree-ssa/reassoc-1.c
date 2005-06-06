/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-optimized" } */
int a, b, c, d;
extern int printf (const char *, ...);
int main(void)
{
  int e;
  int f;
  /* We should be able to transform these into the same expression, and only have two additions.  */
  e = a + b;
  e = e + c;
  f = c + a;
  f = f + b;
  printf ("%d %d\n", e, f);
}

/* { dg-final { scan-tree-dump-times "a \\\+ b" 1 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
