/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* Verify -march will override arch option from -mcpu.  */
/* { dg-options "-mcpu=sifive-u74 -march=rv32ic -mabi=ilp32" } */
/* sifive-s51 = rv64imafdc */

#if !((__riscv_xlen == 32)		\
      && !defined(__riscv_32e)		\
      && !defined(__riscv_mul)		\
      && !defined(__riscv_atomic)	\
      && !defined(__riscv_flen)		\
      && defined(__riscv_compressed))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
