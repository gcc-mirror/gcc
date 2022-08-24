/* Shamelessly plugged off gcc/testsuite/gcc.c-torture/execute/pr28982a.c.  

   The idea is to induce high register pressure for both int/fp registers
   so that they spill. By default FMV instructions would be used to stash
   int reg to a fp reg (and vice-versa) but that could be costlier than
   spilling to stack.  */

/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-march=rv64g -mabi=lp64d -ffast-math" } */

#define NITER 4
#define NVARS 20
#define MULTI(X) \
  X( 0), X( 1), X( 2), X( 3), X( 4), X( 5), X( 6), X( 7), X( 8), X( 9), \
  X(10), X(11), X(12), X(13), X(14), X(15), X(16), X(17), X(18), X(19)

#define DECLAREI(INDEX) inc##INDEX = incs[INDEX]
#define DECLAREF(INDEX) *ptr##INDEX = ptrs[INDEX], result##INDEX = 5
#define LOOP(INDEX) result##INDEX += result##INDEX * (*ptr##INDEX), ptr##INDEX += inc##INDEX
#define COPYOUT(INDEX) results[INDEX] = result##INDEX

double *ptrs[NVARS];
double results[NVARS];
int incs[NVARS];

void __attribute__((noinline))
foo (int n)
{
  int MULTI (DECLAREI);
  double MULTI (DECLAREF);
  while (n--)
    MULTI (LOOP);
  MULTI (COPYOUT);
}

double input[NITER * NVARS];

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

/* { dg-final { scan-assembler-not "\tfmv\\.d\\.x\t" } } */
/* { dg-final { scan-assembler-not "\tfmv\\.x\\.d\t" } } */
