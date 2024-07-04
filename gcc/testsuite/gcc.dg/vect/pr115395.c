/* { dg-additional-options "-mavx2" { target avx2_runtime } } */

#include "tree-vect.h"

struct {
  long header_size;
  long start_offset;
  long end_offset;
} myrar_dbo[5] = {{0, 87, 6980}, {0, 7087, 13980}, {0, 14087, 0}};

int i;
long offset;

int main()
{
  check_vect ();

  offset += myrar_dbo[0].start_offset;
  while (i < 2) {
    i++;
    offset += myrar_dbo[i].start_offset - myrar_dbo[i - 1].end_offset;
  }
  if (offset != 301)
    abort();

  return 0;
}
