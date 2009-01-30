int test_bit(int nr, void *addr)
{
  int *a = (int *)addr;
  int mask;
  a += nr;
  mask = 1 << nr;
  return mask & *a;
}
struct {
    struct {
	int disabled;
    } *data[1];
} trace;
struct {
    unsigned bits[1];
} cpumask;
void inc(int *);
void dec(int *);
int foo(void)
{
  int cpu;
  for (cpu = 0; cpu < 1; cpu++) {
      if (test_bit(cpu, cpumask.bits))
	inc(&trace.data[cpu]->disabled);
      if (!test_bit(cpu, cpumask.bits))
	dec(&trace.data[cpu]->disabled);
  }
}
