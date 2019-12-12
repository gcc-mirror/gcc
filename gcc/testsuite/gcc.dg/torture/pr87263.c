/* { dg-do compile } */

int a, b, c;

int main ()
{ 
  int g, *h[3] = {&g, &g, &g};
  if (h[2] == 0)
    ;
  else
    { 
      int i[1];
      if (a)
	while (a)
	  L:;
      else
	{
	  int k = b;
	}
    }
  if ((b < c) > b)
    goto L;
  return 0;
}
