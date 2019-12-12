/* { dg-do compile } */
/* { dg-additional-options "--param rpo-vn-max-loop-depth=7" } */

int a, b, c;

int main ()
{ 
  int d;
  b = 1;
L1:
  for (; b > 1;)
    goto L2;
    { 
      int e[1];
L3:;
    }
L2:
  while (a)
    { 
      d--;
      goto L1;
    }
  while (c)
    { 
      if (a)
	goto L3;
      if (a)
	break;
      if (a)
	goto L2;
      while (c)
	while (c)
	  while (c)
	    while (c)
	      while (c)
		while (c)
		  while (c)
		    ;
    }
  return 0;
}
