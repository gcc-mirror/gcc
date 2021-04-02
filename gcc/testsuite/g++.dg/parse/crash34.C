/* PR c++/31745 */
/* { dg-do compile }  */

void foo()
{
  namespace N { /* { dg-error "is not allowed" } */
// { dg-error "16:expected" "" { target *-*-* } .-1 }
