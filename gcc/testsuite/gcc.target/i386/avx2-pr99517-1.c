/* PR ipa/99517 */
/* { dg-do run { target avx2 } } */
/* { dg-additional-sources "avx2-pr99517-2.c" } */
/* { dg-options "-O2 -mavx2" } */

#include "avx2-check.h"

typedef signed char v32qi __attribute__((vector_size(32)));
typedef int v4si __attribute__((vector_size(16)));
typedef long long int v4di __attribute__((vector_size(32)));
typedef double v4df __attribute__((vector_size(32)));
extern v32qi foo (v4si);
extern v32qi bar (v4si);

static void
avx2_test (void)
{
  v4si a = { 1, -2, 3, -4 };
  __asm ("" : "+x" (a));
  v4di b = (v4di) bar (a);
  v4df c = (v4df) foo (a);
  if (b[0] != 1 || c[0] != 1.0 || b[1] != -2 || c[1] != -2.0
      || b[2] != 3 || c[2] != 3.0 || b[3] != -4 || c[3] != -4.0)
    __builtin_abort ();
}
