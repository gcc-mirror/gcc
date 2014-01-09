/* { dg-do run } */

extern void abort (void);

int a = 2, b;

int
main ()
{
  int c;
  if (!b)
    {
      b = a;
      c = a == 0 ? 1 : 1 % a;
      if (c)
	b = 0;
    }
  if (b != 0)
    abort ();
  return 0;
}
