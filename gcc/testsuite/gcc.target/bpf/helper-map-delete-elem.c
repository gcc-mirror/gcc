/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <bpf-helpers.h>

char *map () { return 0; }

void
foo ()
{
  int ret;
  char *key = 0;

  ret = bpf_map_delete_elem (map (), key);
}

/* { dg-final { scan-assembler "call\t3" } } */
