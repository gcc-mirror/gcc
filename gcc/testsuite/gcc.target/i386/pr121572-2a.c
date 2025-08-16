/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fplt -mtls-dialect=gnu" } */

typedef enum
{
  MPFR_RNDN
} mpfr_rnd_t;
typedef int mpfr_t[1];
long __gmpfr_emin, mpfr_agm_expo_0;
_Thread_local long __gmpfr_emax;
int mpfr_agm_compare, mpfr_agm___trans_tmp_1;
mpfr_t mpfr_agm_u;
void mpfr_mul (int *, int, int, mpfr_rnd_t);
int
mpfr_agm (int op1)
{
  int op2 = 0;
  if (__builtin_expect (mpfr_agm_compare == 0, 0))
    return 0;
  if (mpfr_agm_compare > 0)
    {
      int t = op1;
      op2 = t;
    }
  mpfr_agm_expo_0 = __gmpfr_emax;
  for (;;)
    {
    retry:
      mpfr_mul (mpfr_agm_u, op1, op2, MPFR_RNDN);
      if (0)
        goto retry;
      if (__builtin_expect (mpfr_agm___trans_tmp_1, 1))
        break;
    }
  __gmpfr_emin = __gmpfr_emax;
  return 0;
}

/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 1 { target { ! ia32 } } } } */
