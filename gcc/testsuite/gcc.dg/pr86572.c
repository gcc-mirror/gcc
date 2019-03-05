/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

const char buf[40] = "test";
void test (int x)
{
  if (__builtin_strlen (buf + x) > 4)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
