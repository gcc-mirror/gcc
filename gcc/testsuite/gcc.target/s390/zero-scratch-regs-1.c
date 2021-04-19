/* { dg-do compile } */
/* { dg-options "-O2 -fzero-call-used-regs=all -march=z13" } */

/* Ensure that all call clobbered GPRs, FPRs, and VRs are zeroed and all call
   saved registers are kept. */

void foo (void) { }

/* { dg-final { scan-assembler-times "lhi\t" 6 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lhi\t%r0,0" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lhi\t%r1,0" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lhi\t%r2,0" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lhi\t%r3,0" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lhi\t%r4,0" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lhi\t%r5,0" { target { ! lp64 } } } } */

/* { dg-final { scan-assembler-times "lzdr\t" 14 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f0" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f1" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f2" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f3" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f5" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f7" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f8" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f9" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f10" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f11" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f12" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f13" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f14" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lzdr\t%f15" { target { ! lp64 } } } } */

/* { dg-final { scan-assembler-times "lghi\t" 6 { target { lp64 } } } } */
/* { dg-final { scan-assembler "lghi\t%r0,0" { target { lp64 } } } } */
/* { dg-final { scan-assembler "lghi\t%r1,0" { target { lp64 } } } } */
/* { dg-final { scan-assembler "lghi\t%r2,0" { target { lp64 } } } } */
/* { dg-final { scan-assembler "lghi\t%r3,0" { target { lp64 } } } } */
/* { dg-final { scan-assembler "lghi\t%r4,0" { target { lp64 } } } } */
/* { dg-final { scan-assembler "lghi\t%r5,0" { target { lp64 } } } } */

/* { dg-final { scan-assembler-times "vzero\t" 24 { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v0" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v1" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v2" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v3" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v4" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v5" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v6" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v7" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v16" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v17" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v18" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v19" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v20" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v21" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v22" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v23" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v24" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v25" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v26" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v27" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v28" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v29" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v30" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v31" { target { lp64 } } } } */
