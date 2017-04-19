/* PR other/61963 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

void f (int * int *a) /* { dg-error "expected" } */
{
    a[0:64] = 0; /* { dg-error "was not declared" "" { target c++ } . } */
    a[0:64] = 0;
}
