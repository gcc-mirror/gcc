/* { dg-do run } */
/* { dg-options "-O" } */

int main()
{
  const long ONE = 1L;
  long y = 0L;
  long x = ((long) (ONE || (y = 1L)) % 8L);
  if (y != 0)
    __builtin_abort ();
  return 0;
}
