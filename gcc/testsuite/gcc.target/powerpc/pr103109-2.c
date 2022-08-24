/* { dg-do run } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target p9modulo_hw } */
/* { dg-require-effective-target has_arch_ppc64 } */

#include "pr103109.h"

union U {
  __int128 i128;
  struct {
    long l1;
    long l2;
  } s;
};

__int128
create_i128 (long most_sig, long least_sig)
{
  union U u;

#if __LITTLE_ENDIAN__
  u.s.l1 = least_sig;
  u.s.l2 = most_sig;
#else
  u.s.l1 = most_sig;
  u.s.l2 = least_sig;
#endif
  return u.i128;
}

#define DEBUG 0

#if DEBUG
#include <stdio.h>
#include <stdlib.h>

void print_i128(__int128 val, int unsignedp)
{
  if (unsignedp)
    printf(" %llu ", (unsigned long long)(val >> 64));
  else
    printf(" %lld ", (signed long long)(val >> 64));

  printf("%llu (0x%llx %llx)",
         (unsigned long long)(val & 0xFFFFFFFFFFFFFFFF),
         (unsigned long long)(val >> 64),
         (unsigned long long)(val & 0xFFFFFFFFFFFFFFFF));
}
#endif

void abort (void);

int main ()
{
  long a = 0xFEDCBA9876543210L;
  long b = 0x1000000L;
  long c = 0x123456L;
  __int128 expected_result = create_i128 (0xFFFFFFFFFFFEDCBAL,
                                          0x9876543210123456L);

  __int128 result = multiply_add (a, b, c);

  if (result != expected_result)
    {
#if DEBUG
      printf ("ERROR: multiply_add (%lld, %lld, %lld) = ", a, b, c);
      print_i128 (result, 0);
      printf ("\n does not match expected_result = ");
      print_i128 (expected_result, 0);
      printf ("\n\n");
#else
      abort();
#endif
    }

  unsigned long au = 0xFEDCBA9876543210UL;
  unsigned long bu = 0x1000000UL;
  unsigned long cu = 0x123456UL;
  unsigned __int128 expected_resultu = create_i128 (0x0000000000FEDCBAL,
                                                    0x9876543210123456L);

  unsigned __int128 resultu = multiply_addu (au, bu, cu);
  if (resultu != expected_resultu)
    {
#if DEBUG
      printf ("ERROR: multiply_addu (%llu, %llu, %llu) = ", au, bu, cu);
      print_i128 (resultu, 1);
      printf ("\n does not match expected_result = ");
      print_i128 (expected_resultu, 1);
      printf ("\n\n");
#else
      abort();
#endif
    }
}
