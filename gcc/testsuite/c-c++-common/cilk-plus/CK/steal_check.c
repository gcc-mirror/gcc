/* { dg-do run  { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-fcilkplus" } */
/* { dg-additional-options "-lcilkrts" { target { i?86-*-* x86_64-*-* } } } */

#ifdef __cplusplus
extern "C" {
#endif

extern int __cilkrts_set_param (const char *, const char *);

#ifdef __cplusplus
}
#endif


void foo(volatile int *);

void main2(void);

int main(void)
{
  /* Ensure more than one worker.  */
  if (__cilkrts_set_param("nworkers", "2") != 0)
    __builtin_abort();

  main2();
  return 0;
}


void main2(void)
{
  int some_var = 0;

  _Cilk_spawn foo(&some_var);

  some_var=1;
  some_var=5;
  some_var=3;
  some_var=4;

  _Cilk_sync; 
  return;
}

void foo(volatile int *some_other_var)
{
  while (*some_other_var == 0)
  {
   ;
  }
}


