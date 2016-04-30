/* Check that these thread relative memory accesses play along with
   surrounding code.
   These should be moved to C torture tests once there are target
   independent thread_pointer built-in functions available.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */

int
test00 (void* p, int x)
{
  int* tcb = (int*)__builtin_thread_pointer ();
  int r = tcb[4];

  __builtin_set_thread_pointer (p);

  tcb = (int*)__builtin_thread_pointer ();
  return tcb[255] + r;
}

int
test01 (void)
{
  unsigned short* tcb = (unsigned short*)__builtin_thread_pointer ();
  return tcb[500];
}

void
test02 (int* x, int a, int b)
{
  int* tcb = (int*)__builtin_thread_pointer ();
  tcb[50] = a;

  __builtin_set_thread_pointer (x);
  
  tcb = (int*)__builtin_thread_pointer ();
  tcb[40] = b;
}

int
test03 (const int* x, int c)
{
  volatile int* tcb = (volatile int*)__builtin_thread_pointer ();

  int s = 0;
  int i;
  for (i = 0; i < c; ++i)
    s ^= x[i] + tcb[40];

  return s;
}

int
test04 (const int* x, int c, int** xx, int d)
{
  int s = 0;
  int i;
  for (i = 0; i < c; ++i)
  {
    volatile int* tcb = (volatile int*)__builtin_thread_pointer ();
    tcb[20] = s;
 
   __builtin_set_thread_pointer (xx[i]);

    tcb = (volatile int*)__builtin_thread_pointer ();
    s ^= x[i] + tcb[40] + d;
  }
  return s;
}
