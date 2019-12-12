/* { dg-do run } */

int x;
int main()
{
  int i = 0;
  x = 0;
  if (x)
    {
      for (; i < 10; ++i)
	{
doit:
	  x = i;
	}
    }
  if (!x)
    goto doit;
  if (x != 9)
    __builtin_abort ();
  return 0;
}
