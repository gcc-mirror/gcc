/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

char *map () { return 0; }

void
foo ()
{
  int ret;
  long long flags = 0;
  char *key = 0, *value = 0;

  ret = __builtin_bpf_helper_map_update_elem (map (), key, value, flags);
}

/* { dg-final { scan-assembler "call\t2" } } */
