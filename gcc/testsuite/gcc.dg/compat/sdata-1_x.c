#include "sdata-section.h"

struct s { int x; int y[4]; };
extern struct s small_struct SDATA_SECTION;

/* Test "load address" operations.  */
int *xaddr (void) { return &small_struct.x; }
int *yaddr (int i) { return &small_struct.y[i]; }

void sdata_1_x (void)
{
  int i;

  /* Test direct accesses.  */
  small_struct.x = 5;
  for (i = 0; i < 4; i++)
    small_struct.y[i] = i + 42;

  if (*xaddr () != 5)
    abort ();

  for (i = 0; i < 4; i++)
    if (*yaddr (i) != i + 42)
      abort ();
}
