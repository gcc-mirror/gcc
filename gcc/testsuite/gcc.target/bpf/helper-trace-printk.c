/* { dg-do compile } */

char *map () { return 0; }

void
foo ()
{
  int ret;

  ret = __builtin_bpf_helper_trace_printk ("foo %d %d", sizeof ("foo %d %d"), 10, 20);
}

/* { dg-final { scan-assembler "call\t6" } } */
