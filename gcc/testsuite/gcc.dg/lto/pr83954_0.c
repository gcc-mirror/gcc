/* { dg-lto-do link } */
#include "pr83954.h"

int main() {
  // just to prevent symbol removal
  FOO_PTR_ARR[1] = 0;
  return 0;
}
