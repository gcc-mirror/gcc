/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

static volatile int v = 0;
static
void benchmark(long runs) {
  void* labels[] = {
    &&l0, &&l1, &&l2
  };
  for(unsigned int mask = 0x1F; mask > 0; mask >>= 1) {
    unsigned lfsr = 0xACE1u;
    long n = 10000000;
    while(n > 0) {
      l2: v;
      l1: v;
      goto *labels[lfsr & mask];
      l0: n--;
    }
  }
}
int f(void) {
  benchmark(10000000);
}
