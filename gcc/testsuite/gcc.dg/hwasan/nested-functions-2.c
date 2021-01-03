/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-shouldfail "hwasan" } */

/*
   Tests of nested funtions are:
    0) Accessing closed over variables works.
    1) Accesses outside of variables is caught.
    2) Accessing variable out of scope is caught.

    Here we test option 2.
 */

#define MAIN 0
#include "nested-functions-0.c"
#undef MAIN

int main ()
{
  int *retval = nested_function (2);
  *retval = 100;
  return 0;
}

/* { dg-output "HWAddressSanitizer: tag-mismatch on address 0x\[0-9a-f\]*.*" } */
/* { dg-output "WRITE of size 4 at 0x\[0-9a-f\]* tags: \[\[:xdigit:\]\]\[\[:xdigit:\]\]/00 \\(ptr/mem\\) in thread T0.*" } */
/* { dg-output "Address 0x\[0-9a-f\]* is located in stack of thread T0.*" } */
/* { dg-output "SUMMARY: HWAddressSanitizer: tag-mismatch \[^\n\]*.*" } */
