/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -mno-vsx" } */

/* Verify compiler emits error message instead of ICE.  */

extern float *dest;
extern __vector_quad src;

int
foo ()
{
  __builtin_mma_disassemble_acc (dest, &src);
  /* { dg-error "'__builtin_mma_disassemble_acc' requires the '-mmma' option" "" { target *-*-* } .-1 } */
  return 0;
}

