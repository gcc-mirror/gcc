/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <bpf-helpers.h>

char *map () { return 0; }

void
foo ()
{
  int ret;
  char *value = 0;

  ret = bpf_map_pop_elem (map (), value);
}

/* { dg-final { scan-assembler "call\t88" } } */
