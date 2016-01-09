/* PR middle-end/50865 */

#define INT64_MIN (-__LONG_LONG_MAX__ - 1)

int
main ()
{
  volatile long long l1 = 1;
  volatile long long l2 = -1;
  volatile long long l3 = -1;

  if ((INT64_MIN % 1LL) != 0)
    __builtin_abort ();
  if ((INT64_MIN % l1) != 0)
    __builtin_abort ();
  if (l2 == -1)
    {
      if ((INT64_MIN % 1LL) != 0)
	__builtin_abort ();
    }
  else if ((INT64_MIN % -l2) != 0)
    __builtin_abort ();
  if ((INT64_MIN % -l3) != 0)
    __builtin_abort ();

  return 0;
}
