/* { dg-prune-output ".*-Wno-abi.*" } */

#include "struct-layout-1.h"

#define TX(n, type, attrs, fields, ops) extern void test##n (void);
#include "struct-layout-1_test.h"
#undef TX

int main (void)
{
#define TX(n, type, attrs, fields, ops)   test##n ();
#include "struct-layout-1_test.h"
#undef TX
  if (fails)
    abort ();
  exit (0);
}
