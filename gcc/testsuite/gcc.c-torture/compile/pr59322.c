
int a, b, d;
short c;

int
foo ()
{
  for (b = 0; b; b = a)
    for (c = 18; c < 10; c++)
      {
	d = c;
	if (d)
	  return 0;
      }
  return 0;
}
