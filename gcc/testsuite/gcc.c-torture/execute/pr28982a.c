/* PR rtl-optimization/28982.  Function foo() does the equivalent of:

     float tmp_results[NVARS];
     for (int i = 0; i < NVARS; i++)
       {
	 int inc = incs[i];
	 float *ptr = ptrs[i], result = 0;
	 for (int j = 0; j < n; j++)
	   result += *ptr, ptr += inc;
	 tmp_results[i] = result;
       }
     memcpy (results, tmp_results, sizeof (results));

   but without the outermost loop.  The idea is to create high register
   pressure and ensure that some INC and PTR variables are spilled.

   On ARM targets, sequences like "result += *ptr, ptr += inc" can
   usually be implemented using (mem (post_modify ...)), and we do
   indeed create such MEMs before reload for this testcase.  However,
   (post_modify ...) is not a valid address for coprocessor loads, so
   for -mfloat-abi=softfp, reload reloads the POST_MODIFY into a base
   register.  GCC did not deal correctly with cases where the base and
   index of the POST_MODIFY are themselves reloaded.  */
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

void __attribute__((noinline))
foo (int n)
{
  int MULTI (DECLAREI);
  float MULTI (DECLAREF);
  while (n--)
    MULTI (LOOP);
  MULTI (COPYOUT);
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
