/* PR tree-optimization/79691 - -Wformat-truncation suppressed by
   (and only by) -Og

   { dg-do compile }
   { dg-options "-Og -Wall -fdump-tree-optimized" } */

char d[2];

/* Verify -Wformat-overflow works.  */
void f1 (void)
{
  __builtin_sprintf (d, "%i", 123);   /* { dg-warning "directive writing 3 bytes" } */
}

/* Verify -Wformat-truncation works.  */
void f2 (void)
{
  __builtin_snprintf (d, sizeof d, "%i", 1234);   /* { dg-warning "output truncated writing 4 bytes" } */
}

/* Verify -fprintf-return-value works.  */
int f3 (void)
{
  return __builtin_snprintf (0, 0, "%i", 12345);
}

/* Verify -fprintf-return-value results used for constant propagation.  */
int f4 (int i)
{
  int n1 = __builtin_snprintf (0, 0, "%i", 1234);
  int n2 = __builtin_snprintf (0, 0, "%i", 12345);
  return n1 + n2;
}

/* { dg-final { scan-tree-dump-times "sprintf" 1 "optimized" } }
   { dg-final { scan-tree-dump-times "snprintf" 1 "optimized" } }
   { dg-final { scan-tree-dump " = 9;" "optimized" } } */
