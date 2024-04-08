/* { dg-options "-O3 -flto -fno-strict-aliasing" }  */

__attribute__ ((noinline))
int
test3 (void *a)
{
  if (!*(void **)a)
          __builtin_abort ();
  return 0;
}

