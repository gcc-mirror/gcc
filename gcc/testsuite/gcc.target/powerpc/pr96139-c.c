/* { dg-do run } */
/* { dg-options "-O2 -Wall -maltivec" } */
/* { dg-require-effective-target powerpc_altivec_ok } */

/*
 * Based on test created by sjmunroe for pr96139
 */

#include <stdio.h>
#include <altivec.h>

volatile vector long long llfoo;

void
print_v2xint64_b () {
  printf (" %016llx \n", llfoo[0]);
  printf (" %016llx \n", llfoo[1]);
}

int 
main() {
llfoo[0]=12345678;
llfoo[1]=34567890;
print_v2xint64_b();
return 0;
}
