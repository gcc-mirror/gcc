/* { dg-do run } */
/* { dg-options "-fno-builtin-memcmp" } */
/* { dg-shouldfail "asan" } */

#include <string.h>

int
main (int argc, char **argv)
{
  char a1[] = {argc, 2, 3, 4};
  char a2[] = {1, 2*argc, 3, 4};
  int res = memcmp (a1, a2, 5 + argc);
  return res;
}

/* { dg-output "ERROR: AddressSanitizer: stack-buffer-overflow.*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ (in _*(interceptor_|)memcmp |\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #1 0x\[0-9a-f\]+ (in _*main|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
