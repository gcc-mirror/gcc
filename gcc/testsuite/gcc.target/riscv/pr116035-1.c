/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64g_zba_xtheadmemidx" { target { rv64 } } } */
/* { dg-options "-march=rv32g_zba_xtheadmemidx" { target { rv32 } } } */

void a(long);
unsigned b[11];
void c()
{
  for (int d = 0; d < 11; ++d)
    a(b[d]);
}

#if __riscv_xlen == 64
unsigned long zext64_32(unsigned int u32)
{
  /* Missed optimization for Zba+XTheadMemIdx.  */
  return u32; //zext.w a0, a0
}
#endif

/* { dg-final { scan-assembler "th.lwuia\t\[a-x0-9\]+,\\(\[a-x0-9\]+\\),4,0" { target rv64 } } } */
/* { dg-final { scan-assembler "th.lwia\t\[a-x0-9\]+,\\(\[a-x0-9\]+\\),4,0" { target rv32 } } } */

/* { dg-final { scan-assembler-not "lwu\t\[a-x0-9\]+,\(\[a-x0-9\]+\),4,0" } } */

/* Missed optimizations for Zba+XTheadMemIdx.  */
/* { dg-final { scan-assembler "zext.w\t" { target rv64 xfail rv64 } } } */

