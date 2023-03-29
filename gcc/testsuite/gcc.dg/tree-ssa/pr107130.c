// { dg-do compile }
// { dg-options "-Os -fno-tree-ccp -fno-tree-forwprop -fno-tree-fre -fdump-tree-vrp1" }

static inline int
foo (int i)
{
  return __builtin_ffsl (i);
}

int
main (void)
{
  int x = foo (41);
  if (x != 1)
    __builtin_abort ();
  return 0;
}

// { dg-final { scan-tree-dump-not "builtin_abort" "vrp1" } }
