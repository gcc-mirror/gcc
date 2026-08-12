/* Verify LUI/ADDI and AUIPC/ADDI fusion for xt-c9501fdvt.  */
/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O2 -mcmodel=medany -mtune=xt-c9501fdvt -mexplicit-relocs -fno-pic -fno-section-anchors -fdump-rtl-sched1" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LUI_ADDI" 1 "sched1" } } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_AUIPC_ADDI" 1 "sched1" } } */

extern int c950_array[];

long
c950_constant (void)
{
  return 0x12345678L;
}

int *
c950_address (void)
{
  return c950_array;
}
