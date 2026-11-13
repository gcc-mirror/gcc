/* Originally a regression test that we don't ICE generating an out-of-bounds
   diagram; now simply a test that we don't ICE.  */

/* { dg-additional-options "-O0 -fdiagnostics-text-art-charset=unicode" } */

#include <stdio.h>
#include <stdint.h>

uint64_t d(int32_t h) {
  uint64_t j[2][6];
  int32_t k;
  for (k = 1;;) {
    printf("FLAG\n");
    if (h < 106 || j[k][h]) /* { dg-warning "use of uninitialized" } */
      return 0;
  }
}
int16_t e() {
  int32_t f[5];
  for (f[2] = 3; f[2]; --f[2])
    d(0);
  return 0;
}

int main() { e(); }

