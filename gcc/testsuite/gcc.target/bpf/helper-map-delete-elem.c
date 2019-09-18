/* { dg-do compile } */

char *map () { return 0; }

void
foo ()
{
  int ret;
  char *key = 0;

  ret = __builtin_bpf_helper_map_delete_elem (map (), key);
}

/* { dg-final { scan-assembler "call\t3" } } */
