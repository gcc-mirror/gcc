// PR gcov-profile/86536
// { dg-options "-fprofile-arcs -ftest-coverage" }
// { dg-do run }
// { dg-require-fork "" }

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int
main (void)
{

  int j = 22;		  /* count(1) */

			  /* returns(200) */
  pid_t pid = fork ();	  /* count(1)  */
			  /* returns(end) */
  if (pid > 0) waitpid (pid, NULL, 0); /* Wait for the child's counts.  */
  int i = 7;		  /* count(2) */
  return 0;		  /* count(2) */
}

// { dg-final { run-gcov branches calls { -b gcov-pr86536.c } } }
