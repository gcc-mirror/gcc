/* PR tree-optimization/108540 */

__attribute__((noipa)) void
bar (const char *cp, unsigned long size, char sign, int dsgn)
{
  if (__builtin_strcmp (cp, "ZERO") != 0 || size != 4 || sign != '-' || dsgn != 1)
    __builtin_abort ();
}

__attribute__((noipa)) void
foo (int x, int ch, double d)
{
  const char *cp = "";
  unsigned long size = 0;
  char sign = '\0';
  switch (x)
    {
    case 42:
      if (__builtin_isinf (d))
	{
	  if (d < 0)
	    sign = '-';
	  cp = "Inf";
	  size = 3;
	  break;
	}
      if (__builtin_isnan (d))
	{
	  cp = "NaN";
	  size = 3;
	  break;
	}
      if (d < 0)
	{
	  d = -d;
	  sign = '-';
	}
      else if (d == 0.0 && __builtin_signbit (d))
	sign = '-';
      else
	sign = '\0';
      if (ch == 'a' || ch == 'A')
	{
	  union U { long long l; double d; } u;
	  int dsgn;
	  u.d = d;
	  if (u.l < 0)
	    {
	      dsgn = 1;
	      u.l &= 0x7fffffffffffffffLL;
	    }
	  else
	    dsgn = 0;
	  if (__builtin_isinf (d))
	    {
	      cp = "INF";
	      size = 3;
	    }
	  else if (__builtin_isnan (d))
	    {
	      cp = "NAN";
	      size = 3;
	    }
	  else if (d == 0)
	    {
	      cp = "ZERO";
	      size = 4;
	    }
	  else
	    {
	      cp = "WRONG";
	      size = 5;
	    }
	  bar (cp, size, sign, dsgn);
	}
    }
}

int
main ()
{
  foo (42, 'a', -0.0);
  return 0;
}
