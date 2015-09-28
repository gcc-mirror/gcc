#include <stdlib.h>
#include <assert.h>

struct abc {
  int a[81];
} *abcd;

#define FPMATH_SSE 2
int global;

void __attribute__ ((noinline)) foo()
{
  int pos = 0;
  int i;

  if (!((global & FPMATH_SSE) != 0))
    for (i = 8; i <= 15; i++)
      abcd->a[pos++] = i;

  for (i = 29; i <= 36; i++)
    abcd->a[pos++] = i;
}

int main()
{
  int i;
  abcd = (struct abc*) malloc (sizeof (struct abc));
  for (i = 0; i <= 80; i++)
    abcd->a[i] = 0;

  foo();

  assert (abcd->a[8] == 29);

  return 0;
}
