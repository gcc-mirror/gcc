/* { dg-do run { target { riscv64*-*-* } } } */
/* { dg-options "-march=rv64gc_zba_zbb -mabi=lp64d" } */

#define myconst 0x4fffaffb0fffefffUL;
volatile unsigned long a = myconst;
unsigned long foo()
{
  return myconst;
}

int main()
{
  if (foo() != a)
    __builtin_abort();
  __builtin_exit (0);
}
