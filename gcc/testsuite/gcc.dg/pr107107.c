/* { dg-do run } */
/* { dg-options "-O2 -ftree-tail-merge" } */

static inline void set_longish(int is_long_long, void *p, long x)
{
  if (is_long_long)
    *(long long*)p = x;
  else
    *(long*)p = x;
}
static long test(long long *p, int index, int mode)
{
  *p = 1;
  set_longish(mode, p+index, 2);
  return *p;
}
long (*volatile vtest)(long long*, int, int) = test;
int main(void)
{
  long long x;
  long result = vtest(&x, 0, 1);
  if (result != 2 || x != 2)
    __builtin_abort ();
  return 0;
}
