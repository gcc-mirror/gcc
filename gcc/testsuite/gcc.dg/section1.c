/* PR optimization/6871 */
/* Constant variables belong in .rodata, not .bss.  */
/* { dg-final { scan-assembler-not "\.bss" { xfail avr-*-*} } } */

const int i = 0;
