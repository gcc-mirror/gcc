/* { dg-do run } */
/* { dg-set-compiler-env-var SOURCE_DATE_EPOCH "630333296" } */

int
main ()
{
  if (__builtin_strcmp (__DATE__, "Dec 22 1989") != 0
      || __builtin_strcmp (__TIME__, "12:34:56") != 0)
    __builtin_abort ();
  return 0;
}
