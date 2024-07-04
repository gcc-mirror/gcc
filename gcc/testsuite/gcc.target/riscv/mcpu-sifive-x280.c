/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=sifive-x280 -mabi=lp64" } */
/* SiFive x280 => rv64imafdcv_zfh_zba_zbb_zvfh_zvl512b */

#if !((__riscv_xlen == 64)		\
      && !defined(__riscv_32e)		\
      && (__riscv_flen == 64)		\
      && defined(__riscv_c)		\
      && defined(__riscv_zfh)		\
      && defined(__riscv_zvfh)		\
      && defined(__riscv_zvl512b)	\
      && defined(__riscv_v))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
