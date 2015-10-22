/* { dg-do run } */
/* With -flto this degenerates to constant folding which doesn't work.  */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-additional-options "-ftrapv" } */
/* { dg-require-effective-target trapping } */
/* { dg-require-fork unused } */

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

/* Verify SImode operations properly trap.  PR middle-end/68046 */

int i = 0x7fffffff;

int main(void)
{
  pid_t child = fork ();
  int status = 0;
  if (child == 0)
    {
      volatile int x = i + 1 < i;
      exit (0);
    }
  else if (child == -1)
    return 0;
  if (wait (&status) == child 
      && status == 0)
    abort ();
  return 0;
}
