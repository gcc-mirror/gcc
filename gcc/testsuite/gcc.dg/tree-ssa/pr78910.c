/* PR tree-optimization/78910 - Wrong print-return-value for a negative number
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized" } */

int main()
{
  char b[128];
  int l = __builtin_sprintf (b, "%.2d", -1);
  __builtin_printf ("b: '%s', length: %d\n", b, l);
  if (l != 3)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "abort" "optimized"} } */
