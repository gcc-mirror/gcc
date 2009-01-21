/* { dg-do run } */
/* { dg-options "-O2 -fmodulo-sched " } */

extern void abort (void);

void foo (int * __restrict__ a, int * __restrict__ b, int * __restrict__ c)
{
   int i;
   for(i = 0; i < 100; i+=4)
     {
       a[i] = b[i] * c[i];
       a[i+1] = b[i+1] * c[i+1];
       a[i+2] = b[i+2] * c[i+2];
       a[i+3] = b[i+3] * c[i+3];
     }
}   


int a[100], b[100], c[100];
int main()
{
  int i, res;
  for(i = 0; i < 100; i++)
    {
      b[i] = c[i] = i;
    }  
  foo(a, b, c);
  
  res = 0;  
  for(i = 0; i < 100; i++)
    {
      res += a[i];
    }
  if(res != 328350)
    abort();
  
  return 0;        
}


