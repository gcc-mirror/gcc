/* Verify that we get the low part of the long long as an int.  We
   used to get it wrong on big-endian machines, if register allocation
   succeeded at all.  We use volatile to make sure the long long is
   actually truncated to int, in case a single register is wide enough
   for a long long.  */
/* { dg-skip-if "asm requires register allocation" { nvptx-*-* } } */
#include <limits.h>

void
ll_to_int (long long x, volatile int *p)
{
  int i;
  asm ("" : "=r" (i) : "0" (x));
  *p = i;
}

int val = INT_MIN + 1;

int main() {
  volatile int i;

  ll_to_int ((long long)val, &i);
  if (i != val)
    abort ();
  
  exit (0);
}
