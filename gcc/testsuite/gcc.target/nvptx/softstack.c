/* { dg-options "-O2 -msoft-stack" } */
/* { dg-do run } */

/* See also 'gcc.target/nvptx/alloca-5.c'.  */

static __attribute__((noinline,noclone)) int f(int *p)
{
  return __sync_lock_test_and_set(p, 1);
}

static __attribute__((noinline,noclone)) int g(int n)
{
  /* Check that variable-length stack allocation works.  */
  int v[n];
  v[0] = 0;
  /* Check that atomic operations can be applied to auto data.  */
  return f(v) == 0 && v[0] == 1;
}

int main()
{
  if (!g(1))
    __builtin_abort();
  return 0;
}
