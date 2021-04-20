/* { dg-do compile } */
/* { dg-options "-O2 -fzero-call-used-regs=all -march=z13 -mzarch" } */

/* Ensure that all call clobbered GPRs, FPRs, and VRs are zeroed and all call
   saved registers are kept. */

void foo (void) { }

/* { dg-final { scan-assembler-times "lghi\t" 6 } } */
/* { dg-final { scan-assembler "lghi\t%r0,0" } } */
/* { dg-final { scan-assembler "lghi\t%r1,0" } } */
/* { dg-final { scan-assembler "lghi\t%r2,0" } } */
/* { dg-final { scan-assembler "lghi\t%r3,0" } } */
/* { dg-final { scan-assembler "lghi\t%r4,0" } } */
/* { dg-final { scan-assembler "lghi\t%r5,0" } } */

/* { dg-final { scan-assembler-times "vzero\t" 30 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "vzero\t" 24 { target {   lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v0" } } */
/* { dg-final { scan-assembler "vzero\t%v1" } } */
/* { dg-final { scan-assembler "vzero\t%v2" } } */
/* { dg-final { scan-assembler "vzero\t%v3" } } */
/* { dg-final { scan-assembler "vzero\t%v4" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v5" } } */
/* { dg-final { scan-assembler "vzero\t%v6" { target { lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v7" } } */
/* { dg-final { scan-assembler "vzero\t%v8"  { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v9"  { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v10" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v11" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v12" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v13" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v14" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v15" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "vzero\t%v16" } } */
/* { dg-final { scan-assembler "vzero\t%v17" } } */
/* { dg-final { scan-assembler "vzero\t%v18" } } */
/* { dg-final { scan-assembler "vzero\t%v19" } } */
/* { dg-final { scan-assembler "vzero\t%v20" } } */
/* { dg-final { scan-assembler "vzero\t%v21" } } */
/* { dg-final { scan-assembler "vzero\t%v22" } } */
/* { dg-final { scan-assembler "vzero\t%v23" } } */
/* { dg-final { scan-assembler "vzero\t%v24" } } */
/* { dg-final { scan-assembler "vzero\t%v25" } } */
/* { dg-final { scan-assembler "vzero\t%v26" } } */
/* { dg-final { scan-assembler "vzero\t%v27" } } */
/* { dg-final { scan-assembler "vzero\t%v28" } } */
/* { dg-final { scan-assembler "vzero\t%v29" } } */
/* { dg-final { scan-assembler "vzero\t%v30" } } */
/* { dg-final { scan-assembler "vzero\t%v31" } } */
