/* { dg-do run { target *-*-linux* *-*-gnu* } } */
/* { dg-shouldfail "ubsan" } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=integer-divide-by-zero" } */

#include <limits.h>
#include <signal.h>
#include <stdlib.h>

int cnt;

__attribute__((noipa)) int
foo (int x, int y)
{
  return x / y;
}

void
handler (int i)
{
  if (cnt++ != 0)
    exit (0);
  volatile int b = foo (5, 0);
  exit (0);
}

int
main (void)
{
  struct sigaction s;
  sigemptyset (&s.sa_mask);
  s.sa_handler = handler;
  s.sa_flags = 0;
  sigaction (SIGFPE, &s, NULL);
  volatile int a = foo (INT_MIN, -1);
  cnt++;
  volatile int b = foo (5, 0);
  return 0;
}

/* { dg-output "division of -2147483648 by -1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division by zero\[^\n\r]*" } */
