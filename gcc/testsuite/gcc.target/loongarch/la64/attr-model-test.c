#ifdef ATTR_MODEL_TEST
int x __attribute__((model("extreme")));
int y __attribute__((model("normal")));
int z;

int
test(void)
{
  return x + y + z;
}

/* The following will be used for kernel per-cpu storage implemention. */

register char *per_cpu_base __asm__("r21");
static int counter __attribute__((section(".data..percpu"), model("extreme")));

void
inc_counter(void)
{
  int *ptr = (int *)(per_cpu_base + (long)&counter);
  (*ptr)++;
}
#endif

int dummy;
