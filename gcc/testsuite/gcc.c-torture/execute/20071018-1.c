extern void abort(void);

struct foo {
  int rank;
  char *name;
};

struct mem {
  struct foo *x[4];
};

void __attribute__((noinline)) bar(struct foo **f)
{
  *f = __builtin_malloc(sizeof(struct foo));
}
struct foo * foo(int rank)
{
  void *x = __builtin_malloc(sizeof(struct mem));
  struct mem *as = x;
  struct foo **upper = &as->x[rank * 8 - 1];
  *upper = 0;
  bar(upper);
  return *upper;
}

int main()
{
  if (foo(0) == 0)
    abort ();
  return 0;
}
