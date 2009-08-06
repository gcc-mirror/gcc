/* { dg-do run } */
/* { dg-skip-if "Variadic funcs have all args on stack. Normal funcs have args in registers." { "avr-*-*" } { "*" } { "" } } */
/* { dg-skip-if "Variadic funcs use Base AAPCS.  Normal funcs use VFP variant." { "arm*-*-*" } { "-mfloat-abi=hard" } { "" } } */

/* PR target/12503 */
/* Origin: <pierre.nguyen-tuong@asim.lip6.fr> */

/* Verify that __builtin_apply behaves correctly on targets
   with pre-pushed arguments (e.g. SPARC).  */

   

#define INTEGER_ARG  5

extern void abort(void);

void foo(char *name, double d, double e, double f, int g)
{
  if (g != INTEGER_ARG)
    abort();
}

void bar(char *name, ...)
{
  __builtin_apply(foo, __builtin_apply_args(), 64);
}

int main(void)
{
  bar("eeee", 5.444567, 8.90765, 4.567789, INTEGER_ARG);

  return 0;
}
