static int mumble;

void
f (void)
{
  mumble = 41;
}

int __attribute__((noinline))
g (void)
{
  return ++mumble;
}
