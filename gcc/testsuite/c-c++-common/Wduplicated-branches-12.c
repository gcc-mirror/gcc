/* PR c/64279 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches" } */

void
f (int i)
{
  if (i) /* { dg-warning "this condition has identical branches" } */
    return 0;
/* { dg-warning ".return. with a value" "" { target c } .-1 } */
/* { dg-error "return-statement with a value" "" { target c++ } .-2 } */
  else
   return 0;
/* { dg-warning ".return. with a value" "" { target c } .-1 } */
/* { dg-error "return-statement with a value" "" { target c++ } .-2 } */
}
