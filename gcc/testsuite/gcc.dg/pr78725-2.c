/* { dg-do run } */
/* { dg-options "-O3 -fsplit-loops" } */

int a, b, c;

int main ()
{
  int d;
  for (; c < 1; c++)
    for (d = 0; d < 3; d++)
      for (b = 0; b < 1; b++)
	if (c >= d)
	  a = 1;

  if (a != 1)
    __builtin_abort ();

  return 0;
}
