/* { dg-do compile } */

typedef long unsigned int size_t;
extern __attribute__ ((malloc)) void *mem_alloc(size_t);
void *mem_alloc(size_t amount)
{
  void *q = __builtin_malloc (amount);
  if (!q) {
      __builtin_printf("malloc");
      __builtin_exit(255);
  }
}
void mem_realloc()
{
  mem_alloc(1);
}
void put_env_var()
{
  mem_alloc(1);
}
