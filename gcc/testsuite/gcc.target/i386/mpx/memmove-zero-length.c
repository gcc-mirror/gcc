/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"
#include "string.h"

int mpx_test (int argc, const char **argv)
{
  int *buf = (int *)malloc (100 * sizeof(int));

  memmove (buf, NULL, 0);
  memmove (NULL, buf, 0);

  free (buf);

  return 0;
}
