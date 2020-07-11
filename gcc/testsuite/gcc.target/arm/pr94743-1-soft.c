/* PR target/94743 */
/* { dg-do compile } */
/* Thumb1 mode not supported for interrupt routines.  */
/* { dg-require-effective-target arm32 } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } {"-mfloat-abi=soft" } } */
/* { dg-options "-mfloat-abi=soft" } */

/* Check that we do not emit a warning when compiling an IRQ handler without
   -mgeneral-regs-only with -mfloat-abi=soft.  */
typedef struct {
  double fpdata[32];
} dummy_t;

dummy_t global_d;
dummy_t global_d1;

/* This function may clobber VFP registers.  */
__attribute__ ((interrupt("IRQ"))) void IRQ_HDLR_Test(void)
{
  global_d.fpdata[3] += global_d.fpdata[3] * global_d1.fpdata[3];
}

/* This function does not need to clobber VFP registers.  */
__attribute__ ((interrupt("IRQ"))) void IRQ_HDLR_Test2(void)
{
  global_d.fpdata[3] = 1.0;
}
