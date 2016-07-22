/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */
/* { dg-options "-fcilkplus" } */

#ifdef __cplusplus
extern "C" {
#endif

extern int __cilkrts_set_param (const char *, const char *);

#ifdef __cplusplus
}
#endif


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
  /* Ensure more than one worker.  */
  if (__cilkrts_set_param("nworkers", "2") != 0)
    __builtin_abort();

  f3();
  return 0;
}
