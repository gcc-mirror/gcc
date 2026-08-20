/* { dg-do compile } */
/* { dg-options "-march=unset -mcpu=sifive-p550 -mabi=lp64d" } */
/* SiFive p550 => rv64imafdc_zifencei_zfhmin_zba_zbb */

#if !((__riscv_xlen == 64)		\
      && !defined(__riscv_32e)		\
      && (__riscv_flen == 64)		\
      && defined(__riscv_c)		\
      && defined(__riscv_zifencei)	\
      && defined(__riscv_zfhmin)	\
      && defined(__riscv_zba)		\
      && defined(__riscv_zbb))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
