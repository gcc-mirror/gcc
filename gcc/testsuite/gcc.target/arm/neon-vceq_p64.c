/* { dg-do run } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"
#include <stdio.h>

extern void abort (void);

int
main (void)
{
  uint64_t args[] = { 0x0, 0xdeadbeef, ~0xdeadbeef, 0xffff,
                      ~0xffff, 0xffffffff, ~0xffffffff, ~0x0 };
  int i, j;

  for (i = 0; i < sizeof (args) / sizeof (args[0]); ++i)
    {
       for (j = 0; j < sizeof (args) / sizeof (args[0]); ++j)
         {
           uint64_t a1 = args[i];
           uint64_t a2 = args[j];
           uint64_t res = vceq_p64 (vreinterpret_p64_u64 (a1),
                                    vreinterpret_p64_u64 (a2));
           uint64_t exp = (a1 == a2) ? ~0x0 : 0x0;

           if (res != exp)
             {
               fprintf (stderr, "vceq_p64 (a1= %lx, a2= %lx)"
                                " returned %lx, expected %lx\n",
                                 a1, a2, res, exp);
               abort ();
             }
         }
    }
  return 0;
}
