struct foo {
  char placeholder[5];
};

void *
test (const char *str)
{
  struct foo *p = (struct foo *) __builtin_malloc(sizeof(struct foo));
  if (!p)
    return p;

  __builtin_memset(p, 0, sizeof(*p));
  
  static int s = 1;
  __atomic_store_n(&s, 0, 0);

  return p;
}
