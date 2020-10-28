/* { dg-do run } */

long long e, *d = &e;
int a, b, c;

int
main ()
{
  for (; c <= 5; c++)
    for (b = 0; b <= 5; b++)
      {
	for (a = 1; a <= 5; a++)
	  ;
	*d = 0;
	if (c)
	  break;
      }
  if (a != 6)
    __builtin_abort ();
  return 0;
}
