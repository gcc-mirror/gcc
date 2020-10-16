/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=sifive-e34 -mabi=ilp32" } */
/* sifive-e34 = rv32imafc */

#if !((__riscv_xlen == 32)		\
      && !defined(__riscv_32e)		\
      && defined(__riscv_mul)		\
      && defined(__riscv_atomic)	\
      && (__riscv_flen == 32)		\
      && defined(__riscv_compressed))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
