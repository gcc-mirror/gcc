/* { dg-do compile } */
/* { dg-options "-Os -mno-unaligned-access" } */
/* { dg-final { scan-assembler "ldrb" } } */
/* { dg-final { scan-assembler "strb" } } */

struct s
{
  char u;
  long long v[2];
} __attribute__((packed,aligned(1)));

__attribute__((noinline, noclone))
long long foo(struct s *x, int y, long long z)
{
  long long a = x->v[y];
  x->v[y] = z;
  return a;
}

struct s a = {0,{0,0}};
int main()
{
  if (foo(&a,0,1) != 0)
    __builtin_abort();
  if (foo(&a,0,2) != 1)
    __builtin_abort();
  if (foo(&a,1,1) != 0)
    __builtin_abort();
  return 0;
}
