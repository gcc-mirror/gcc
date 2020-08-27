/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <bpf-helpers.h>

char *map () { return 0; }

void
foo ()
{
  char *key = 0, *value = 0;
  value = bpf_map_lookup_elem (map (), key);
}

/* { dg-final { scan-assembler "call\t1" } } */
