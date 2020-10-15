/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=sifive-e20 -mabi=ilp32" } */
/* sifive-e20 = rv32imc */

#if !((__riscv_xlen == 32)		\
      && !defined(__riscv_32e)		\
      && defined(__riscv_mul)		\
      && !defined(__riscv_atomic)	\
      && !defined(__riscv_flen)		\
      && defined(__riscv_compressed))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
