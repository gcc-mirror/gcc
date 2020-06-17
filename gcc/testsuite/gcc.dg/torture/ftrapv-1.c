/* { dg-do run } */
/* { dg-additional-options "-ftrapv -fno-ipa-vrp" } */
/* { dg-require-effective-target trapping } */
/* { dg-require-fork "" } */

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

/* Verify SImode operations properly trap.  PR middle-end/52478  */

/* Disallow inlining/cloning which would constant propagate and trigger
   unrelated bugs.  */

int __attribute__((noipa))
iaddv (int a, int b)
{
  return a + b;
}

int main(void)
{
  pid_t child = fork ();
  int status = 0;
  if (child == 0)
    {
      volatile int x = iaddv (__INT_MAX__, 1);
      exit (0);
    }
  else if (child == -1)
    return 0;
  if (wait (&status) == child 
      && status == 0)
    abort ();
  return 0;
}
