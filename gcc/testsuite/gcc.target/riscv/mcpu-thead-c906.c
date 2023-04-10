/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=thead-c906" { target { rv64 } } } */
/* T-Head XuanTie C906 => rv64imafdc */

#if !((__riscv_xlen == 64)		\
      && !defined(__riscv_32e)		\
      && defined(__riscv_mul)		\
      && defined(__riscv_atomic)	\
      && (__riscv_flen == 64)		\
      && defined(__riscv_compressed)	\
      && defined(__riscv_xtheadba)	\
      && defined(__riscv_xtheadbb)	\
      && defined(__riscv_xtheadbs)	\
      && defined(__riscv_xtheadcmo)	\
      && defined(__riscv_xtheadcondmov)	\
      && defined(__riscv_xtheadfmemidx)	\
      && defined(__riscv_xtheadmac)	\
      && defined(__riscv_xtheadmemidx)	\
      && defined(__riscv_xtheadmempair)	\
      && defined(__riscv_xtheadsync))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
