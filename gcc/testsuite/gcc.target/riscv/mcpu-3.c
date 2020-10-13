/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=sifive-s51 -mabi=lp64" } */
/* sifive-s51 = rv64imac */

#if !((__riscv_xlen == 64)		\
      && !defined(__riscv_32e)		\
      && defined(__riscv_mul)		\
      && defined(__riscv_atomic)	\
      && !defined(__riscv_flen)		\
      && defined(__riscv_compressed))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
