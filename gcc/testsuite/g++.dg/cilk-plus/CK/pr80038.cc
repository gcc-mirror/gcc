/* { dg-options "-fcilkplus" } */
/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */

#include <unistd.h>
extern "C" {
  extern int __cilkrts_set_param (const char *, const char *);
}

int objcnt = 0;

struct foo
{
  int live;
  foo ()
    { objcnt++; }
  foo (const foo &)
    { objcnt++; }
  ~foo ()
    { objcnt--; }
};

void
spawnee (foo f)
{
  usleep(2000);
  /* Now both my_test::f and spawnee::f should be alive.  */
  if (objcnt != 2)
    __builtin_abort ();
}

void
my_test ()
{
  foo f;
  _Cilk_spawn spawnee (f);
  _Cilk_sync ;
}

int
main ()
{
  if (__cilkrts_set_param ("nworkers", "2") != 0)
    __builtin_abort ();

  my_test ();
}
