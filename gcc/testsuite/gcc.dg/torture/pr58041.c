/* { dg-do run } */

typedef long long V
  __attribute__ ((vector_size (2 * sizeof (long long)), may_alias));

struct s
{
  char u;
  V v[2];
} __attribute__((packed,aligned(1)));

__attribute__((noinline, noclone))
long long foo(struct s *x, int y, V *z)
{
  V a = x->v[y];
  x->v[y] = *z;
  return a[1];
}

struct s a = {0,{{0,0},{0,0}}};
int main()
{
  V v1 = {0,1};
  V v2 = {0,2};

  if (foo(&a,0,&v1) != 0)
    __builtin_abort();
  if (foo(&a,0,&v2) != 1)
    __builtin_abort();
  if (foo(&a,1,&v1) != 0)
    __builtin_abort();
  return 0;
}
