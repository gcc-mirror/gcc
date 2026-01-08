/* PR middle-end/111817 */
/* { dg-do compile } */

int
foo (int x, int y)
{
  asm ("" : "\n=g" (x) : "0" (y));	/* { dg-warning "output constraint '=' for operand 0 is not at the beginning" } */
  return x;				/* { dg-error "invalid punctuation '\\\\x\[0-9a-zA-Z]*' in constraint" "" { target *-*-* } .-1 } */
}
