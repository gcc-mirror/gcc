/* PR middle-end/101950 */
/* { dg-do compile } */
/* { dg-options "-O2 -mlzcnt" } */
/* { dg-final { scan-assembler-not "call\[^\n\r]*__clrsb.i2" } } */
/* { dg-final { scan-assembler-times "\tlzcnt\[ql]\t" 2 } } */
/* { dg-final { scan-assembler-times "\txor\[ql]\t" 2 } } */
/* { dg-final { scan-assembler-times "\tsar\[ql]\t|\tcltd" 2 } } */

/* Use pointers to avoid register allocation difference due to argument
   and return register being different and the difference in selecting eax
   for one the result of the xor vs selecting rdi due to the order of the
   shift vs the not shift. */

int
foo (long *x)
{
  return __builtin_clrsbl (*x);
}

int
bar (int *x)
{
  return __builtin_clrsb (*x);
}
