/* { dg-do run } */

enum tst { first = 0, second = 1 };

int
main ()
{
  enum tst data[16];

  for (unsigned i = 0; i < 16; i++)
    data[i] = (i < 5 ? second : first);

  if (data[2] != second)
    __builtin_abort ();
  return 0;
}
