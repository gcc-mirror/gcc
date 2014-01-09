/* { dg-do run  { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-fcilkplus" } */
/* { dg-additional-options "-lcilkrts" { target { i?86-*-* x86_64-*-* } } } */

void f0(volatile int *steal_flag)
{ 
  int i = 0;
  /* Wait for steal_flag to be set */
  while (!*steal_flag) 
    ;
}

int f1()
{

  volatile int steal_flag = 0;
  _Cilk_spawn f0(&steal_flag);
  steal_flag = 1;  // Indicate stolen
  _Cilk_sync; 
  return 0;
}

void f2(int q)
{
  q = 5;
}

void f3()
{
   _Cilk_spawn f2(f1());
}

int main()
{
  f3();
  return 0;
}
