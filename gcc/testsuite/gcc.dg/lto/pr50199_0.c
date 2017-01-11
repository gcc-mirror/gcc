/* PR middle-end/50199 */
/* { dg-lto-options {{-O2 -flto -fno-merge-constants --param=lto-min-partition=1}} } */

__attribute__ ((noinline)) const char *
foo (const char *x)
{
  return x;
}

int
main ()
{
  const char *a = "ab";
  if (a != foo (a))
    __builtin_abort ();
  return 0;
}
