/* { dg-do compile } */

unsigned int a, b, c;

void
foo (unsigned int x)
{
  do
    {
      if (a == 0 ? 1 : 1 % a)
	for (; b; b--)
	  lab:;
      else
	while (x)
	  ;
      if (c)
	goto lab;
    }
  while (1);
}
