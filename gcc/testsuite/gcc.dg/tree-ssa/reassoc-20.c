/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned int a, b, c, d;
extern int printf (const char *, ...);
int main(void)
{
  unsigned int e;
  unsigned int f;
  /* We should be able to transform these into the same expression, and only have two additions.  */
  e = a + b;
  e = e + c;
  f = c + a;
  f = f + b;
  printf ("%d %d\n", e, f);
}

/* { dg-final { scan-tree-dump-times "b.._. \\\+ a.._." 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\\+ " 2 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
