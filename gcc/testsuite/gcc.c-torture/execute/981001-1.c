void abort (void);
void exit (int);

#define NG   0x100L

unsigned long flg = 0;

long sub (int n)
{
  int a, b ;

  if (n >= 2)
    {
      if (n % 2 == 0)
	{
	  a = sub (n / 2);
	  
	  return (a + 2 * sub (n / 2 - 1)) * a;
	}
      else
	{
	  a = sub (n / 2 + 1);
	  b = sub (n / 2);
	  
	  return a * a + b * b;
	}
    }
  else 
    return (long) n;
}

int main (void)
{
  if (sub (30) != 832040L)
    flg |= NG;

  if (flg)
    abort ();
  
  exit (0);
}
