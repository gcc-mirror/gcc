/* { dg-do run } */

struct b {
  char *volatile c;
};
struct b * __attribute__((noipa))
d()
{
  char *e;
  struct b *b = __builtin_malloc(sizeof(b));
  void *f = __builtin_malloc(1);

  e = __builtin_memcpy(f, "z", 1);
  b->c = e;
  return b;
}

int main()
{
  struct b b = *d();
  if (b.c[0] != 'z')
    __builtin_abort();
  return 0;
}
