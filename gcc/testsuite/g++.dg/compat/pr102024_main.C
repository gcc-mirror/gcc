/* { dg-require-effective-target int32plus } */
/* { dg-options "-Wno-abi" } */
/* { dg-options "-mno-mmx -Wno-abi" { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fno-common" { target hppa*-*-hpux* powerpc*-*-darwin* } } */
/* { dg-options "-mno-mmx -fno-common -Wno-abi" { target i?86-*-darwin* x86_64-*-darwin* } } */
/* { dg-options "-mno-base-addresses" { target mmix-*-* } } */
/* { dg-options "-mlongcalls -mtext-section-literals" { target xtensa*-*-* } } */

#include "struct-layout-1.h"

#define TX(n, type, attrs, fields, ops) extern void test##n (void);
#include "pr102024_test.h"
#undef TX

int main (void)
{
#define TX(n, type, attrs, fields, ops)   test##n ();
#include "pr102024_test.h"
#undef TX
  if (fails)
    {
      fflush (stdout);
      abort ();
    }
  exit (0);
}
