/* PR c/61077 */
/* { dg-do compile } */
/* { dg-options "-std=c11 -Wall" } */

_Atomic int
main (_Atomic int argc, _Atomic char **argv)
/* { dg-warning "qualified return type" "return" { target *-*-* } 6 } */
/* { dg-warning "qualified parameter type.*int" "parameter" { target *-*-* } 6 } */
/* { dg-warning "qualified parameter type.*char" "parameter" { target *-*-* } 6 } */
{
  return 0;
}
