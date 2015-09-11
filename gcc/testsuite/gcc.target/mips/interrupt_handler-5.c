/* Test the interrupt handler with an accumulator.  */
/* { dg-do assemble } */
/* { dg-options "-mips64r2" } */
_Accum a;
__attribute__((interrupt))
void foo () {
  a = a*a;
}
