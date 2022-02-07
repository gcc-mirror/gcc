/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -mmma -mno-vsx" } */

/* Verify the emitted error message.  */

extern float *dest;
extern __vector_quad src;

int
foo ()
{
  __builtin_mma_disassemble_acc (dest, &src);
  /* { dg-error "'-mmma' requires '-mvsx'" "mma" { target *-*-* } 0 } */
  return 0;
}

