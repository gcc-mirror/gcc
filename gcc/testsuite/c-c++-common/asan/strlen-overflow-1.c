/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-shouldfail "asan" } */

#include <sanitizer/asan_interface.h>

char a[2] = "0";

#ifdef __cplusplus
extern "C"
#endif
__SIZE_TYPE__
strlen (const char *p);

int main () {
  char *p = &a[0];
  asm ("" : "+r"(p));
  __asan_poison_memory_region ((char *)&a[1], 1);
  return __builtin_strlen (a) + 1;
}

/* { dg-output "READ of size 2 at 0x\[0-9a-f\]+ thread T0.*(\n|\r\n|\r)" } */
/* { dg-output "    #1 0x\[0-9a-f\]+ +(in _*main (\[^\n\r]*strlen-overflow-1.c:19|\[^\n\r]*:0)|\[(\]).*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*0x\[0-9a-f\]+ is located 0 bytes to the right of global variable" } */
