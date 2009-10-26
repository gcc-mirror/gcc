/* { dg-lto-do link } */
/* { dg-lto-options {{-fwhopr -r -nostdlib -fPIC}} } */
#include "20081224_0.h"

extern struct foo x;

void f(void) {
  x.x = 0;
}
