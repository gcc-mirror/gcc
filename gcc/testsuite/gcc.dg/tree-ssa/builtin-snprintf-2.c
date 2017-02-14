/* PR middle-end/79496 - call to snprintf with non-zero size eliminated
   with -Wformat-truncation=2
   { dg-do compile }
   { dg-options "-O2 -Wall -Wformat-truncation=2 -fprintf-return-value -fdump-tree-optimized" } */

char d[2];

int test_cst (unsigned n)
{
  if (1 < n)
    n = 0;

  return __builtin_snprintf (d, n, "%d", 1);
}

int test_var (char *d, unsigned n)
{
  if (2 < n)
    n = 0;

  return __builtin_snprintf (d, n, "%i", 1);
}

/* { dg-final { scan-tree-dump-times "snprintf" 2 "optimized"} } */
