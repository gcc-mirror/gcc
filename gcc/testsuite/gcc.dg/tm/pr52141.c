/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O0 -w" } */

__attribute__((always_inline))
static void asmfunc(void)
{
  __asm__ (""); /* { dg-error "'asm' not allowed in 'transaction_safe" } */
}

__attribute__((transaction_safe))
static void f(void)
{
  asmfunc();
}

int main()
{
  __transaction_atomic {
    f();
  }
  return 0;
}

/* { dg-message "inlined from \'f\'" "" { target *-*-* } 0 } */
