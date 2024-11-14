/* { dg-do compile } */
/* { dg-options "-Wmissing-parameter-name" } */

int
f (int) /* { dg-warning "omitting parameter names" } */
{
}
