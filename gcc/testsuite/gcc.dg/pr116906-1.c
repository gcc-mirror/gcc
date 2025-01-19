/* { dg-require-effective-target alarm } */
/* { dg-require-effective-target signal } */
/* { dg-options "-O2" } */

#include <stdint.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>

int a = 1, b = 0;

uint64_t safe_mod(uint64_t a, uint64_t b)
{
    if (b == 0) return a;
    else return a % b;
}

int __attribute__((noipa))
f(uint64_t p)
{
    int c = 0;
j:
    b = safe_mod(
        (c = ((a &= (0 < p)) && 1), 1), p);
    if (!c)
        goto j;
    return 0;
}

void do_exit (int i)
{
  exit (0);
}

int main()
{
  struct sigaction s;
  sigemptyset (&s.sa_mask);
  s.sa_handler = do_exit;
  s.sa_flags = 0;
  sigaction (SIGALRM, &s, NULL);
  alarm (1);
  f(b);
}
