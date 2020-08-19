/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <bpf-helpers.h>

char *map () { return 0; }

void
foo ()
{
  int ret;

  ret = bpf_trace_printk ("foo %d %d", sizeof ("foo %d %d"), 10, 20);
}

/* { dg-final { scan-assembler "call\t6" } } */
