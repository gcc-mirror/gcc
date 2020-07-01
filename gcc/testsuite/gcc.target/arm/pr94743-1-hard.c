/* PR target/94743 */
/* { dg-do compile } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } {"-mfloat-abi=hard" } } */
/* { dg-require-effective-target arm_vfp_ok } */
/* { dg-add-options arm_vfp } */
/* Make sure with use -mfloat-abi=hard.  */
/* { dg-additional-options "-mfloat-abi=hard" } */

/* Check that we emit a warning when compiling an IRQ handler without
   -mgeneral-regs-only.  */
typedef struct {
  double fpdata[32];
} dummy_t;

dummy_t global_d;
dummy_t global_d1;

/* This function may clobber VFP registers.  */
__attribute__ ((interrupt("IRQ"))) void IRQ_HDLR_Test(void)
{ /* { dg-warning { FP registers might be clobbered despite 'interrupt' attribute: compile with '-mgeneral-regs-only'} "" { target *-*-* } . } */
  global_d.fpdata[3] += global_d.fpdata[3] * global_d1.fpdata[3];
}

/* This function does not need to clobber VFP registers.  */
/* Do we want to emit a (useless?) warning?  */
__attribute__ ((interrupt("IRQ"))) void IRQ_HDLR_Test2(void)
{ /* { dg-warning { FP registers might be clobbered despite 'interrupt' attribute: compile with '-mgeneral-regs-only'} "" { target *-*-* } . } */
  global_d.fpdata[3] = 1.0;
}
