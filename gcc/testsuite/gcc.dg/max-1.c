/* PR middle-end/18548 */
/* Test case reduced by Andrew Pinski <pinskia@physics.uc.edu> */
/* { dg-do run } */
/* { dg-options "-O1 -fno-tree-lrs" } */

extern void abort (void);

long fff[10];

void f(long a, long b)
{
  long crcc = b;
  long d = *((long*)(a+1));
  int i;

  a = d >= b? d:b;


  for(i=0;i<10;i++)
   fff[i] = a;
}

int main(void)
{
  int i;
  long a = 10;
  f((long)(&a)-1,0);
  for(i = 0;i<10;i++)
   if (fff[i]!=10)
    abort ();
  return 0;
}

