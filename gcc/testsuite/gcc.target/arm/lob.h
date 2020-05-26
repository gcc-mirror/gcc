#include <string.h>

/* Common code for lob tests.  */

#define NO_LOB asm volatile ("@ clobber lr" : : : "lr" )

#define N 10000

static void
reset_data (int *a, int *b, int *c)
{
  memset (a, -1, N * sizeof (*a));
  memset (b, -1, N * sizeof (*b));
  memset (c, -1, N * sizeof (*c));
}
