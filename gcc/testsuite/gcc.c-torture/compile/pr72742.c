/* PR target/72742 */

int a, b;
unsigned short int c;

void
foo (int x, unsigned short int *y)
{
  int fx;
 lab:
    {
      unsigned short int va;
      if (x != 0)
	{
	  c %= a < 0;
	  while (c < 17)
	    ++c;
	  b &= fx;
	  if ((a & (b != 0 ? *y : 0)) != 0)
	    {
	      va /= 3;
	      a += (va != 0) ? (va = a) : 0;
	    }
	  a = va && a;
	  goto lab;
	  y = &va;
	}
    }
}

void
bar (int x, unsigned short int *y)
{
  int fx;
 lab:
    {
      unsigned short int va;
      if (x != 0)
	{
	  c %= a < 0;
	  while (c < 17)
	    ++c;
	  b &= fx;
	  if ((a & (b != 0 ? *y : 24)) != 0)
	    {
	      va /= 3;
	      a += (va != 0) ? (va = a) : 0;
	    }
	  a = va && a;
	  goto lab;
	  y = &va;
	}
    }
}

void
baz (int x, unsigned short int *y)
{
  int fx;
 lab:
    {
      unsigned short int va;
      if (x != 0)
	{
	  c %= a < 0;
	  while (c < 17)
	    ++c;
	  b &= fx;
	  if ((a & (b != 0 ? *y : 25)) != 0)
	    {
	      va /= 3;
	      a += (va != 0) ? (va = a) : 0;
	    }
	  a = va && a;
	  goto lab;
	  y = &va;
	}
    }
}
