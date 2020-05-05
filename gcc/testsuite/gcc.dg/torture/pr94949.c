/* { dg-do run } */
/* { dg-additional-options "-fallow-store-data-races" } */

static int x = 1;
static volatile int y = -1;
int
main()
{
  for (int i = 0; i < 128; ++i)
    {
      if (i == y)
	x = i;
    }
  if (x != 1)
    __builtin_abort ();
  return 0;
}
