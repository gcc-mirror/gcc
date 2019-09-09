/* { dg-do compile } */

char *map () { return 0; }

void
foo ()
{
  int ret;
  char *value = 0;

  ret = __builtin_bpf_helper_map_pop_elem (map (), value);
}

/* { dg-final { scan-assembler "call\t88" } } */
