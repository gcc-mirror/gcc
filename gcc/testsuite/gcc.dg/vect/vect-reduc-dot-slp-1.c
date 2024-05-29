/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0 -fdump-tree-optimized" } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target arm_v8_2a_dotprod_neon_hw { target { aarch64*-*-* || arm*-*-* } } } */
/* { dg-add-options arm_v8_2a_dotprod_neon }  */

#include "tree-vect.h"

#ifndef SIGNEDNESS_1
#define SIGNEDNESS_1 signed
#define SIGNEDNESS_2 signed
#endif

SIGNEDNESS_1 int __attribute__ ((noipa))
f (SIGNEDNESS_1 int res0,
   SIGNEDNESS_1 int res1,
   SIGNEDNESS_1 int res2,
   SIGNEDNESS_1 int res3,
   SIGNEDNESS_1 int res4,
   SIGNEDNESS_1 int res5,
   SIGNEDNESS_1 int res6,
   SIGNEDNESS_1 int res7,
   SIGNEDNESS_1 int res8,
   SIGNEDNESS_1 int res9,
   SIGNEDNESS_1 int resA,
   SIGNEDNESS_1 int resB,
   SIGNEDNESS_1 int resC,
   SIGNEDNESS_1 int resD,
   SIGNEDNESS_1 int resE,
   SIGNEDNESS_1 int resF,
   SIGNEDNESS_2 char *a,
   SIGNEDNESS_2 char *b)
{
  for (int i = 0; i < 64; i += 16)
    {
      res0 += a[i + 0x00] * b[i + 0x00];
      res1 += a[i + 0x01] * b[i + 0x01];
      res2 += a[i + 0x02] * b[i + 0x02];
      res3 += a[i + 0x03] * b[i + 0x03];
      res4 += a[i + 0x04] * b[i + 0x04];
      res5 += a[i + 0x05] * b[i + 0x05];
      res6 += a[i + 0x06] * b[i + 0x06];
      res7 += a[i + 0x07] * b[i + 0x07];
      res8 += a[i + 0x08] * b[i + 0x08];
      res9 += a[i + 0x09] * b[i + 0x09];
      resA += a[i + 0x0A] * b[i + 0x0A];
      resB += a[i + 0x0B] * b[i + 0x0B];
      resC += a[i + 0x0C] * b[i + 0x0C];
      resD += a[i + 0x0D] * b[i + 0x0D];
      resE += a[i + 0x0E] * b[i + 0x0E];
      resF += a[i + 0x0F] * b[i + 0x0F];
    }

  return res0 ^ res1 ^ res2 ^ res3 ^ res4 ^ res5 ^ res6 ^ res7 ^
         res8 ^ res9 ^ resA ^ resB ^ resC ^ resD ^ resE ^ resF;
}

/* { dg-final { scan-tree-dump "vect_recog_dot_prod_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump-not "DOT_PROD_EXPR" "optimized" } } */
