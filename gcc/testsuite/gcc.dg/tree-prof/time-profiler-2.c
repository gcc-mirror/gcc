/* { dg-options "-O2 -fdump-ipa-profile" } */

#include <unistd.h>

__attribute__ ((noinline))
int foo()
{
  return 1;
}

__attribute__ ((noinline))
int bar()
{
  return 1;
}

__attribute__ ((noinline))
int baz()
{
  return 1;
}

__attribute__ ((noinline))
int baz1()
{
  return 1;
}

int main ()
{
  int f = fork();
  int r = 0;

  foo ();

  if (f < 0)
    return 1; /* Fork failed.  */

  if(f == 0) /* Child process.  */
    r = bar() - foo();
  else /* Parent process.  */
    r = foo() - foo();

  return r;
}
/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Read tp_first_run: 0" 2 "profile"} } */
/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Read tp_first_run: 1" 1 "profile"} } */
/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Read tp_first_run: 2" 1 "profile"} } */
/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Read tp_first_run: 3" 1 "profile"} } */
