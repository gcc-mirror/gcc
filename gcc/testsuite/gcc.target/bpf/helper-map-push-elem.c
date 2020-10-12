/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <bpf-helpers.h>

char *map () { return 0; }

void
foo ()
{
  int ret;
  char *value = 0;
  long long flags = 0;

  ret = bpf_map_push_elem (map (), value, flags);
}

/* { dg-final { scan-assembler "call\t87" } } */
