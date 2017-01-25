/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

/* Fix for PR78631 */

#define SHOULDFAIL

#include <stdio.h>
#include <string.h>
#include "mpx-check.h"

char s[10];
char d[10];
__attribute__((noinline))

char* foo(char* dst, char* src, size_t size) {
  return memcpy(dst, src, size);
}
int mpx_test(int argc, const char **argv) {
  char* r = foo(d, s, 11);
  printf("r = %p\n", r);
  return 0;
}
