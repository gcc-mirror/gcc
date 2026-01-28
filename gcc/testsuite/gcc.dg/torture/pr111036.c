/* { dg-do run } */

#include <stdlib.h>

__attribute__((aligned(32))) static struct
{
  unsigned long long available_cmd_ids_per_core[2];
} _rl2c_cmd_id_data;

static inline void __attribute__((always_inline))
foo (void *base, size_t length)
{
  unsigned long int p = (unsigned long int) base;
  if (__builtin_constant_p(p) && (p & 31) == 0) { exit (0); } 
  else if (__builtin_constant_p(length)) { exit (0); } 
  else { exit (0); }
}

int main(int argc, char **argv)
{
  foo(&_rl2c_cmd_id_data, sizeof(*(&_rl2c_cmd_id_data)));
  return 0;
}

