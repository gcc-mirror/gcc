/* { dg-do run } */

/* Make sure a bit-field store of 0 cause the whole assignment become 0. */

struct s1
{
  unsigned char d:7;
  unsigned char c:1;
};

__attribute__((noinline))
struct s1 f(struct s1 a)
{
  a.c = 0;
  struct s1 t = a;
  return t;
}

int main()
{
  struct s1 a = {2, 1};
  struct s1 b = f(a);
  if (b.c != 0)
    __builtin_abort();
  if (b.d != 2)
    __builtin_abort();
  return 0;
}
