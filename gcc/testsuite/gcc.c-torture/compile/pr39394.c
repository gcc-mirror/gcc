/* PR tree-optimization/39394 */

char *p;
int x;

static inline void
f1 (int n)
{
  asm volatile ("" : "=m" (*(struct { char x[n]; } *) p));
}

static inline void
f2 (void)
{
  x ? f1 (1) : f1 (2);
}

static inline void
f3 (void)
{
  f2 ();
}

void
f4 (void)
{
  f3 ();
}
