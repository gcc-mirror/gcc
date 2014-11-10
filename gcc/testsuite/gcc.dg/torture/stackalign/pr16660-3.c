/* { dg-do run } */
/* { dg-skip-if "Stack alignment causes use of alloca" { nvptx-*-* } "*" "" } */

#include "check.h"

typedef __SIZE_TYPE__ size_t;
#define ALIGNMENT 256
int main(void)
{
  int a[ALIGNMENT/sizeof(int)] __attribute__((aligned(ALIGNMENT)));
  check (&a, ALIGNMENT);
  int b[ALIGNMENT/sizeof(int)] __attribute__((aligned(ALIGNMENT)));
  check (&b, ALIGNMENT);
  return 0;
}
