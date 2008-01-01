/* { dg-do compile } */
/* { dg-options "-Wmain -mstdmain" } */

int main (void *wrong)/* { dg-warning "first argument of 'main' should be 'int'" "" } */
{ 
  /* { dg-warning "'main' takes only zero or two arguments" "" { target *-*-* } 4 } */
}
