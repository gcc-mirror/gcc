/* { dg-do run } */
/* { dg-options "-O1" } */ 

void abort (void);

int f(int i1243)
{
  int i[2], *i1 = i;
  i[0] = 1;
  volatile int *i2 = i1;
  i2[1] = 1;
  i1243 = 0;
  return i2[1]+i2[0];
}


int main(void)
{
  if( f(100) != 2)
   abort ();
  return 0;
}
