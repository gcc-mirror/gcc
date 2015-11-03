/* { dg-do run } */
/* { dg-require-effective-target arm_eabi } */
/* { dg-options "" } */

/* Check that long long divmod functions pass the right argument to
   __aeabi_ldiv0 on divide by zero.  */

#ifdef DEBUGME
#include <stdio.h>
#else
extern void abort (void);
#endif

/* Override div zero handler and simply return the provided value.  */
long long __aeabi_ldiv0 (long long r)
{
  return r;
}

long long lldiv (long long a, long long b)
{
  return a / b;
}

unsigned long long ulldiv (unsigned long long a, unsigned long long b)
{
  return a / b;
}

void check (long long num, long long expected)
{
  long long res = lldiv (num, 0LL);
  if (res != expected)
#ifdef DEBUGME
    {
      printf ("num=%08X:%08X\n", (unsigned)(num >> 32), (unsigned)num);
      printf ("res=%08X:%08X\n", (unsigned)(res >> 32), (unsigned)res);
    }
#else
    abort ();
#endif
}

void ucheck (unsigned long long num, unsigned long long expected)
{
  unsigned long long res = ulldiv (num, 0ULL);
  if (res != expected)
#ifdef DEBUGME
    {
      printf ("num=%08X:%08X\n", (unsigned)(num >> 32), (unsigned)num);
      printf ("res=%08X:%08X\n", (unsigned)(res >> 32), (unsigned)res);
    }
#else
    abort ();
#endif
}

#define POS_BIG 0x7fffffffffffffffLL
#define NEG_BIG 0x8000000000000000LL
#define UNS_BIG 0xffffffffffffffffULL

int main ()
{
  check (0LL, 0LL);
  check (1LL, POS_BIG);
  check (0x000000007fffffffLL, POS_BIG);
  check (0x00000000ffffffffLL, POS_BIG);
  check (0x0000000100000000LL, POS_BIG);
  check (POS_BIG, POS_BIG);
  check (-1LL, NEG_BIG);
  check (-0x000000007fffffffLL, NEG_BIG);
  check (-0x00000000ffffffffLL, NEG_BIG);
  check (-0x0000000100000000LL, NEG_BIG);
  check (NEG_BIG, NEG_BIG);

  ucheck (0ULL, 0ULL);
  ucheck (1ULL, UNS_BIG);
  ucheck (0x000000007fffffffULL, UNS_BIG);
  ucheck (0x00000000ffffffffULL, UNS_BIG);
  ucheck (0x0000000100000000ULL, UNS_BIG);
  ucheck ((unsigned long long)POS_BIG, UNS_BIG);
  ucheck (UNS_BIG, UNS_BIG);

  return 0;
}
