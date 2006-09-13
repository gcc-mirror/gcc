/* Like pr28982a.c, but with the spill slots outside the range of
   a single sp-based load on ARM.  This test tests for cases where
   the addresses in the base and index reloads require further reloads.  */
#if defined(STACK_SIZE) && STACK_SIZE <= 0x80100
int main (void) { return 0; }
#else
#define NITER 4
#define NVARS 20
#define MULTI(X) \
  X( 0), X( 1), X( 2), X( 3), X( 4), X( 5), X( 6), X( 7), X( 8), X( 9), \
  X(10), X(11), X(12), X(13), X(14), X(15), X(16), X(17), X(18), X(19)

#define DECLAREI(INDEX) inc##INDEX = incs[INDEX]
#define DECLAREF(INDEX) *ptr##INDEX = ptrs[INDEX], result##INDEX = 0
#define LOOP(INDEX) result##INDEX += *ptr##INDEX, ptr##INDEX += inc##INDEX
#define COPYOUT(INDEX) results[INDEX] = result##INDEX

float *ptrs[NVARS];
float results[NVARS];
int incs[NVARS];

struct big { int i[0x10000]; };
void __attribute__((noinline))
bar (struct big b)
{
  incs[0] += b.i[0];
}

void __attribute__((noinline))
foo (int n)
{
  struct big b = {};
  int MULTI (DECLAREI);
  float MULTI (DECLAREF);
  while (n--)
    MULTI (LOOP);
  MULTI (COPYOUT);
  bar (b);
}

float input[NITER * NVARS];

int
main (void)
{
  int i;

  for (i = 0; i < NVARS; i++)
    ptrs[i] = input + i, incs[i] = i;
  for (i = 0; i < NITER * NVARS; i++)
    input[i] = i;
  foo (NITER);
  for (i = 0; i < NVARS; i++)
    if (results[i] != i * NITER * (NITER + 1) / 2)
      return 1;
  return 0;
}
#endif
