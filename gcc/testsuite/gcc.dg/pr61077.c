/* PR c/61077 */
/* { dg-do compile } */
/* { dg-options "-std=c11 -Wall" } */

_Atomic int
main (_Atomic int argc, _Atomic char **argv)
/* { dg-warning "qualified return type" "return" { target *-*-* } .-1 } */
/* { dg-warning "qualified parameter type\[^\n\]*int" "parameter" { target *-*-* } .-2 } */
/* { dg-warning "qualified parameter type\[^\n\]*char" "parameter" { target *-*-* } .-3 } */
{
  return 0;
}
