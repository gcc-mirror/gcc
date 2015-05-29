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

/* We cannot reassociate these expressions because of undefined signed
   integer overflow.  Instead the value-numberer has to be extended
   to canonicalize these expressions.  */

/* { dg-final { scan-tree-dump-times "b.._. \\\+ a.._." 1 "optimized" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times " \\\+ " 2 "optimized" { xfail *-*-* } } } */
