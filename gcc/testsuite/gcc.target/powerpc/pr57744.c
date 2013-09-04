/* { dg-do run { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8 -O3" } */

void abort (void);

typedef unsigned U_16 __attribute__((mode(TI)));

extern int libat_compare_exchange_16 (U_16 *, U_16 *, U_16, int, int)
  __attribute__((__noinline__));

/* PR 57744: lqarx/stqcx needs even/odd register pairs.  The assembler will
   complain if the compiler gets an odd/even register pair.  Create a function
   which has the 16 byte compare and exchange instructions, but don't actually
   execute it, so that we can detect these failures on older machines. */

int
libat_compare_exchange_16 (U_16 *mptr, U_16 *eptr, U_16 newval,
         int smodel, int fmodel __attribute__((unused)))
{
  if (((smodel) == 0))
    return __atomic_compare_exchange_n (mptr, eptr, newval, 0, 0, 0);
  else if (((smodel) != 5))
    return __atomic_compare_exchange_n (mptr, eptr, newval, 0, 4, 0);
  else
    return __atomic_compare_exchange_n (mptr, eptr, newval, 0, 5, 0);
}

U_16 a = 1, b = 1, c = -2;
volatile int do_test = 0;

int main (void)
{
  if (do_test && !libat_compare_exchange_16 (&a, &b, c, 0, 0))
    abort ();

  return 0;
}
