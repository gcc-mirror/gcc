int n;

void foo (int i)
{
  int a, b;

  if (!i)
    for (a = 1; a < 3; a++)
      if (a)
	for (b = 1; b < 3; b++)
	  foo (b);

  n++;
}

