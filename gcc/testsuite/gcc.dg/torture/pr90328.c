/* { dg-do run } */

void g(int*__restrict x, int*y)
{
  *x = *y;
}

void __attribute__((noipa)) f(int* a,int* b)
{
  for(int i=0;i<1024;++i)
    g(a+i,b+i);
}

int main()
{
  int x[1025];
  for (int i = 0; i < 1025; ++i)
    x[i] = i+1;
  f(x+1, x);
  for (int i = 0; i < 1025; ++i)
    if (x[i] != 1)
      __builtin_abort ();
  return 0;
}
