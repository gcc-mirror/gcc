/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-optimized -ffast-math" } */
float a, b, c, d;
extern int printf (const char *, ...);
int main(void)
{
  float e;
  float f;
  /* We should not be able to transform these into the same expression, and only have two additions.  */
  e = a + b;
  e = e + c;
  f = c + a;
  f = f + b;
  printf ("%f %f\n", e, f);
}

/* { dg-final { scan-tree-dump-times "\\\+" 2 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
