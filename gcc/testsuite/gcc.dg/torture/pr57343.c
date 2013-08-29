/* { dg-do run } */

int c = 0;

int
main ()
{
  int i, f = 1;
  for (i = 0; i < 5; i++)
    {
      --c;
      unsigned char h = c * 100;
      if (h == 0)
	{
	  f = 0;
	  break;
	}
    }
  if (f != 1)
    __builtin_abort ();
  return 0;
}
