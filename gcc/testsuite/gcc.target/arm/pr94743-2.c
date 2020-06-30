/* PR target/94743 */
/* { dg-do compile } */
/* Thumb1 mode not supported for interrupt routines.  */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-mgeneral-regs-only" } */

/* Check that we do not emit a warning when compiling an IRQ handler
   with -mgeneral-regs-only.  */
typedef struct {
  /* Do not use floating-point types, which are not compatible with
     -mgeneral-regs-only under -mfloat-abi=hard */
  int fpdata[32];
} dummy_t;

dummy_t global_d;
dummy_t global_d1;

/* This function does not clobber VFP registers.  */
__attribute__ ((interrupt("IRQ"))) void IRQ_HDLR_Test(void)
{
  global_d.fpdata[3] += global_d.fpdata[3] * global_d1.fpdata[3];
}
