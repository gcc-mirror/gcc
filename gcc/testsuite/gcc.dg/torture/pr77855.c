/* { dg-do run } */

int a, b = 1, c, e, f, g, k, m, n, o;
char d, h, i, j, l; 
char res[2];

void __attribute__ ((noinline,noclone)) fn2 ()
{
  d = 2;
}

void fn3 ()
{
  for (;;)
    {
      for (; b; b--)
	{
	  fn2 ();  
	  if (e)
	    j = 1;
	  if (f)
	    L1:
		k = j | (a & l);
	  for (;;)
	    {
	      __builtin_snprintf (res, 2, "%d\n", d);
	      if (d)
		break;
	      for (; o; o--)
		for (; n;)
		  for (; m; m++)
		    ;
	      goto L1;
	    }
	}
      g = h;
      c = i;
      break;
    }
}

int main ()
{
  fn3 ();
  if (res[0] != '2')
    __builtin_abort ();
  return 0; 
}
