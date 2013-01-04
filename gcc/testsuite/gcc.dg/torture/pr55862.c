/* { dg-do compile } */

int g, a, *b;

void f(void)
{
  int *p;

  if(g)
    {
      int **k = &p;

      for(; g; p++)
	for(a = 0; a < 1; a++)
	  {
	    int *c = p;
label2:
	    if(a < 1)
	      *c = 0;
	  }

      goto label1;

      while(g++)
	for(*b = 0; *b; b++)
	  label1:
	      ;
    }

  goto label2;
}
