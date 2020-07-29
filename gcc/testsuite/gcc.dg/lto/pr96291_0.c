/* { dg-lto-do link } */

#include "pr96291.h"

static void * b;
void c(int d) {
  f();
  a(b, b);
}

void e(void) { c(0); }
