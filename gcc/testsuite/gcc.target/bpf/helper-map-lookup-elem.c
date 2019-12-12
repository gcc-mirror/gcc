/* { dg-do compile } */

char *map () { return 0; }

void
foo ()
{
  char *key = 0, *value = 0;
  value = __builtin_bpf_helper_map_lookup_elem (map (), key);
}

/* { dg-final { scan-assembler "call\t1" } } */
