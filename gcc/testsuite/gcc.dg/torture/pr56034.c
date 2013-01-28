/* { dg-do compile } */
/* { dg-options "-ftree-loop-distribution" } */

int a, b, *p;

void f(void)
{
  int *q;

  while(b++)
    {
      int i;
      p = &i;
      a = *q;
    }

  if(a)
    for(;; b++);
}
