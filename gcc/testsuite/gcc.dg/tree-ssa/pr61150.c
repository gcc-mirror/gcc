/* { dg-do run } */
/* { dg-options "-O3" } */

int a, b, c, d = 1;

int
main ()
{
  int e = d;
  for (b = 0; b < 5; b++)
  {
    for (a = 0; a < 1; a++)
    {
      if (e)
	break;
      for (c = 0; c < 1; c++)
	;
    }
    e |= 1;
  }
  if (c)
    __builtin_abort();
  return 0;
}
