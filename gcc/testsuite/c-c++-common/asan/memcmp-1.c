/* { dg-do run } */
/* { dg-options "-fno-builtin-memcmp" } */
/* { dg-additional-options "-D_FORTIFY_SOURCE=0 -gdwarf-3" { target *-*-darwin* } } */
/* { dg-shouldfail "asan" } */

#include <string.h>

volatile int one = 1;

int
main ()
{
  char a1[] = {(char)one, 2, 3, 4};
  char a2[] = {1, (char)(2*one), 3, 4};
  int res = memcmp (a1, a2, 5 + one);
  return res;
}

/* { dg-output "ERROR: AddressSanitizer: stack-buffer-overflow.*(\n|\r\n|\r)" } */
/* { dg-output "    #\[1-9\] 0x\[0-9a-f\]+ +(in _*(interceptor_|wrap_|)memcmp|\[(\])\[^\n\r]*(\n|\r\n|\r)" { target { ! *-*-darwin* } } } */
/* { dg-output "    #\[2-9\] 0x\[0-9a-f\]+ +(in _*main|\[(\])\[^\n\r]*(\n|\r\n|\r)" { target { ! *-*-darwin* } } } */

/* { dg-output "    #\[0-9\] 0x\[0-9a-f\]+ +(in MemcmpInterceptorCommon|\[(\])\[^\n\r]*(\n|\r\n|\r)" { target *-*-darwin* } } */
/* { dg-output "    #\[1-9\] 0x\[0-9a-f\]+ +(in wrap_(memcmp|bcmp)|\[(\])\[^\n\r]*(\n|\r\n|\r)" { target *-*-darwin* } } */
