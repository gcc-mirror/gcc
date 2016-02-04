/* { dg-do compile } */

int a, b, c;

int
main ()
{
  for (; c; c++)
    for (a = 0; a < 4; a++)
      {
	c &= 5;
	for (b = 0; b < 2; b++)
	  c |= 1;
      }
  return 0; 
}
