/* { dg-do compile } */
/* { dg-require-effective-target riscv_native_cpu_detect } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-mcpu=native -march=rv64gc -mabi=lp64d" } */

#ifdef __riscv_v
#error "-mcpu=native overrode an explicit -march"
#endif

int
main (void)
{
  return 0;
}
