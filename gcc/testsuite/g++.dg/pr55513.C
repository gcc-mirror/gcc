// { dg-do compile }
// { dg-options "-O0 -fdump-tree-gimple" }

int
main ()
{
  char s[10];
  const int t = (__builtin_memcpy (s, "Hello", 6), 777);
  __builtin_printf ("%d %s\n", t, s);
  return 0;
}

// { dg-final { scan-tree-dump-times "memcpy" 1 "gimple" } }
