/* Verify we don't ICE generating out-of-bounds diagram.  */

/* { dg-additional-options "-O0 -fdiagnostics-text-art-charset=unicode" } */

#include <stdio.h>
#include <stdint.h>

uint64_t d(int32_t h) {
  uint64_t j[2][6];
  int32_t k;
  for (k = 1;;) {
    printf("FLAG\n");
    if (h < 106 || j[k][h]) /* { dg-warning "stack-based buffer over-read" } */
      return 0;
  }
}
int16_t e() {
  int32_t f[5];
  for (f[2] = 3; f[2]; --f[2])
    d(0);
}

int main() { e(); }

/* We don't care about the exact diagram, just that we don't ICE.  */

/* { dg-allow-blank-lines-in-output 1 } */
/* { dg-prune-output ".*" } */
