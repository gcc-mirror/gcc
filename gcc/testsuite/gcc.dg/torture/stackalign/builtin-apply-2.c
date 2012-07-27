/* PR target/12503 */
/* Origin: <pierre.nguyen-tuong@asim.lip6.fr> */

/* Verify that __builtin_apply behaves correctly on targets
   with pre-pushed arguments (e.g. SPARC).  */

/* { dg-do run } */

/* { dg-skip-if "Variadic funcs use Base AAPCS.  Normal funcs use VFP variant." { arm_hf_eabi } } */
   

#define INTEGER_ARG  5

#if defined(__ARM_PCS) || defined(__epiphany__)
/* For Base AAPCS, NAME is passed in r0.  D is passed in r2 and r3.
   E, F and G are passed on stack.  So the size of the stack argument
   data is 20.  */
#define STACK_ARGUMENTS_SIZE  20
#else
#define STACK_ARGUMENTS_SIZE  64
#endif

extern void abort(void);

void foo(char *name, double d, double e, double f, int g)
{
  if (g != INTEGER_ARG)
    abort();
}

void bar(char *name, ...)
{
  __builtin_apply(foo, __builtin_apply_args(), STACK_ARGUMENTS_SIZE);
}

int main(void)
{
  bar("eeee", 5.444567, 8.90765, 4.567789, INTEGER_ARG);

  return 0;
}
