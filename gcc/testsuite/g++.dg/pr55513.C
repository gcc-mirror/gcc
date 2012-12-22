// { dg-do compile }
// { dg-options "-O0 -fdump-tree-gimple" }

main ()
{
  char s[10];
  const int t = (__builtin_memcpy (s, "Hello", 6), 777);
  __builtin_printf ("%d %s\n", t, s);
}

// { dg-final { scan-tree-dump-times "memcpy" 1 "gimple" } }
// { dg-final { cleanup-tree-dump "gimple" } }
