/* { dg-do run } */
/* { dg-options "-O -fdump-tree-ifcombine-details" } */

/* Check that we can optimize misaligned double-words.  */

#include <stddef.h>

struct s {
  short a;
  long long b;
  int c;
  long long d;
  short e;
} __attribute__ ((packed, aligned (8)));

struct s p = { 0, 0, 0, 0, 0 };

__attribute__ ((__noinline__, __noipa__, __noclone__))
int fp ()
{
  if (p.a
      || p.b
      || p.c
      || p.d
      || p.e)
    return 1;
  else
    return -1;
}

int main () {
  /* Unlikely, but play safe.  */
  if (sizeof (long long) == sizeof (short))
    return 0;
  if (fp () > 0)
    __builtin_abort ();
  unsigned char *pc = (unsigned char *)&p;
  for (int i = 0; i < offsetof (struct s, e) + sizeof (p.e); i++)
    {
      pc[i] = 1;
      if (fp () < 0)
	__builtin_abort ();
      pc[i] = 0;
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "optimizing" 4 "ifcombine" { target { ! { avr-*-* pru-*-* } } } } } */
