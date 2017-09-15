/* { dg-do compile } */

int a, b, c;

void fn1 ()
{ 
  while (1)
    { 
      if (c)
	goto L2;
      break;
    }
  if (c)
    {
L1:
	{
	  int g[1];
	  if (b)
	    goto L1;
	  goto L1;
L2:
	  for (a = 0; a;)
	    goto L1;
	}
    }
}

int main ()
{ 
  fn1 ();
  return 0;
}
