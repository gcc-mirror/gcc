/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O1" } */

inline void asmfunc(void)
{
  __asm__ (""); /* { dg-error "asm not allowed in .transaction_safe" } */
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
