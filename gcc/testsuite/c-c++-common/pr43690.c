/* PR middle-end/43690 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (char *x)
{
  asm ("" : : "m" (x++));	/* { dg-error "is not directly addressable" } */
  asm ("" : : "m" (++x));	/* { dg-error "is not directly addressable" "" { target c } } */
  asm ("" : : "m" (x--));	/* { dg-error "is not directly addressable" } */
  asm ("" : : "m" (--x));	/* { dg-error "is not directly addressable" "" { target c } } */
  asm ("" : : "m" (x + 1));	/* { dg-error "is not directly addressable" } */
}
