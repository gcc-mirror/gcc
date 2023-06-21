extern void foo (float, float *, float *);

void
bar (void *p)
{
  float *__attribute__((aligned (64))) q = __builtin_assume_aligned (p, 64);
  foo (0.0f, q, q);
}
