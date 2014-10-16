/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-shouldfail "asan" } */

#include <stdbool.h>
#include <sanitizer/asan_interface.h>

char a[2] = "0";

#ifdef __cplusplus
extern "C"
#endif

__attribute__((no_sanitize_address, noinline)) __SIZE_TYPE__
strlen (const char *p) {

  __SIZE_TYPE__ n = 0;
  for (; *p; ++n, ++p);
  return n;
}

int main () {
  char *p = &a[0];
  asm ("" : "+r"(p));
  __asan_poison_memory_region ((char *)&a[1], 1);
  return __builtin_strlen (a);
}

/* { dg-output "READ of size 1 at 0x\[0-9a-f\]+ thread T0.*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ (in _*main (\[^\n\r]*strlen-overflow-1.c:26|\[^\n\r]*:0)|\[(\]).*(\n|\r\n|\r)" } */
