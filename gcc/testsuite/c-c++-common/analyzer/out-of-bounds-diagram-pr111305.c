/* Verify we don't ICE generating out-of-bounds diagram.  */

/* { dg-additional-options "-O0 -fdiagnostics-text-art-charset=unicode" } */

#include <stdio.h>
#include <stdint.h>

struct a {
  uint32_t b;
};
union c {
  int8_t b;
};

int32_t *d( int32_t *j, int32_t k, struct a l) {
  int64_t m[1]= {0};
  for (l.b = 0; l.b <= 0; l.b++) {
    printf("FLAG\n");
    l.b == 12 && m[l.b]; /* { dg-bogus "stack-based buffer over-read" } */
  }
}

/* We don't care about the exact diagram, just that we don't ICE.  */

/* { dg-allow-blank-lines-in-output 1 } */
/* { dg-prune-output ".*" } */
