/* PR target/94743 */
/* { dg-do compile } */
/* Thumb1 mode not supported for interrupt routines.  */
/* { dg-require-effective-target arm32 } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } {"-mfloat-abi=soft" } } */
/* { dg-options "-mgeneral-regs-only -mfloat-abi=soft" } */

/* Check that we do not emit a warning when compiling an IRQ handler
   with -mgeneral-regs-only even when using floating-point data.  */
typedef struct {
  /* Since we use -mfloat=abi=soft, this will generate calls to
     libgcc, but won't clobber VFP registers.  */
  double fpdata[32];
} dummy_t;

dummy_t global_d;
dummy_t global_d1;

/* This function does not clobber VFP registers.  */
__attribute__ ((interrupt("IRQ"))) void IRQ_HDLR_Test(void)
{
  global_d.fpdata[3] += global_d.fpdata[3] * global_d1.fpdata[3];
}
