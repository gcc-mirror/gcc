/* { dg-do compile } */

int a;

void f(void)
{
  int b;

  if(0)
    lbl:
	goto lbl;

  if(b)
    {
      int p = 0;
      goto lbl;
    }

  a = 0;
  while(b++);
  goto lbl;
}
