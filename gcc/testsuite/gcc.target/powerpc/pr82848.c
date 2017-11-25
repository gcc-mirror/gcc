/* PR target/82848 */
/* { dg-do compile } */
/* { dg-options "-mno-altivec -mno-vsx -Wno-psabi" } */

#define C 3.68249351546114573519399405666776E-44f
#define vector __attribute__ ((altivec (vector__)))

vector float
foo (vector float a)
{
  vector float b = __builtin_vec_madd (b, a, (vector float) { C, C, C, C });	/* { dg-error "requires the '-maltivec' option" } */
  return b;
}
