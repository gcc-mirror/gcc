/* { dg-do compile } */

int a, c;

void f(void)
{
  unsigned char b;

  if(a)
    {
      for(; b < 1; b++);
lbl1:
      c = (b |= 0) ^ (b || a);
    }

  if((a = b))
    {
      b = c;
      goto lbl1;
    }

  b = 5;
  goto lbl1;
}
