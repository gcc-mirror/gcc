/* { dg-options "-Wall -DDEFINED" } */

int
foo ()
{
}  /* { dg-warning "control" "" { target def_nocache } } */
