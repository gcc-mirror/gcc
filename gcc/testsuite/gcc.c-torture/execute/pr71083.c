__extension__ typedef __UINT32_TYPE__ uint32_t;

struct lock_chain {
  uint32_t irq_context: 2,
    depth: 6,
    base: 24;
};

__attribute__((noinline, noclone))
struct lock_chain * foo (struct lock_chain *chain)
{
  int i;
  for (i = 0; i < 100; i++)
    {
      chain[i+1].base = chain[i].base;
    }
  return chain;
}

struct lock_chain1 {
  char x;
  unsigned short base;
} __attribute__((packed));

__attribute__((noinline, noclone))
struct lock_chain1 * bar (struct lock_chain1 *chain)
{
  int i;
  for (i = 0; i < 100; i++)
    {
      chain[i+1].base = chain[i].base;
    }
  return chain;
}

struct lock_chain test [101];
struct lock_chain1 test1 [101];

int
main ()
{
  foo (test);
  bar (test1);
  return 0;
}
