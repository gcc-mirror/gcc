/* PR middle-end/19068 */
/* Test case by Andrew Pinski <pinskia@physics.uc.edu> */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern "C" void abort (void);

long fff[10];

void f(long a)
{
  int i;
  a =  *((long*)(a+1+sizeof(long))) >? *((long*)(a+1)); 

  for(i=0;i<10;i++)
   fff[i] = a;
}

int main(void)
{
  int i;
  long a[2] = {10,5};
  f((long)(&a)-1);
  for(i = 0;i<10;i++)
   if (fff[i]!=10)
    abort ();
  return 0;
}

