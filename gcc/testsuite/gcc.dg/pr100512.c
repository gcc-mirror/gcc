/* { dg-do compile } */
/* { dg-options "-O2 -w" } */

#include <stdint.h>
int a;
void b() {
  int16_t *c;
  uint16_t d = 2;
  if (0 == d) {
    uint64_t e;
    uint64_t *f = &e;
    for (;;) {
      if (e += 0 >= 0)
        for (;;)
          ;
    g:
      for (; a;) {
        int16_t i = &d;
        *c = i && *f;
      }
    }
  }
  goto g;
}

