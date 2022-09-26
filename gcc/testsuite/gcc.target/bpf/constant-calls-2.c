/* { dg-do compile } */
/* { dg-options "-std=c89 -O2" } */

static void * (*bpf_map_lookup_elem)(void *map, const void *key) = (void *) 666;

int foo ()
{
  char *ret;

  ret = bpf_map_lookup_elem (ret, ret);
  if (ret)
    return 0;
  return 1;
}

/* { dg-final { scan-assembler "call\t666" } } */
