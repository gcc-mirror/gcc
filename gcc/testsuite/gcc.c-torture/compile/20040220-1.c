/* PR 14194 */

int irqs;

static inline __attribute__((always_inline))
int kstat_irqs (void) {
  int i, sum = 0;
  for (i = 0; i < 1; i++)
    if (__builtin_expect(i, 0))
      sum += irqs;
  return sum;
}

int show_interrupts (void) {
  return kstat_irqs ();
}
