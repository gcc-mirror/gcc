/* { dg-do compile } */

void f (char *c, ...)
{
  __builtin_next_arg (*c); /* { dg-warning "not last named argument" } */
}
