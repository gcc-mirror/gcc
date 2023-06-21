/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-options "-fno-builtin-malloc -fno-builtin-free" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */
/* { dg-shouldfail "hwasan" } */

#include <stdio.h>
#ifdef __cplusplus
extern "C" {
#endif
extern void *malloc (__SIZE_TYPE__);
extern void free (void *);
#ifdef __cplusplus
}
#endif

struct Simple {
  int value;
};

int f(struct Simple simple) {
  return simple.value;
}

int main() {
  struct Simple *psimple = (struct Simple *) malloc(sizeof(struct Simple));
  psimple->value = 42;
  free(psimple);
  printf("%d\n", f(*psimple));
  return 0;
}

/* { dg-output "HWAddressSanitizer: tag-mismatch on address 0x\[0-9a-f\]*.*" } */
/* { dg-output "READ of size 4 at 0x\[0-9a-f\]* tags: \[\[:xdigit:\]\]\[\[:xdigit:\]\]/\[\[:xdigit:\]\]\[\[:xdigit:\]\] \\(ptr/mem\\) in thread T0.*" } */
/* { dg-output "freed by thread T0 here:.*" } */
/* { dg-output "previously allocated by thread T0 here:" } */
