/* PR c/58346 */
/* { dg-do compile } */

void
foo (void)
{
  __PTRDIFF_TYPE__ d;
  const int i = 0;
  int a1[2][0], a2[2][0];
  int b1[3][i], b2[4][i];
  d = a1 - a2; /* { dg-error "arithmetic on pointer to an empty aggregate" } */
  __asm volatile ("" : "+g" (d));
  /* No error here for C.  */
  d = b1 - b2; /* { dg-error "arithmetic on pointer to an empty aggregate" "" { target c++ } } */
  __asm volatile ("" : "+g" (d));
}
