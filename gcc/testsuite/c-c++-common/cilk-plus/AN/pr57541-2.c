/* PR middle-end/57541 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

void foo1 ()
{
  int a;
  a = __sec_reduce_add (1); /* { dg-error "Invalid builtin arguments" } */
}

void foo2 ()
{
  int a;
  a = __sec_reduce_add (); /* { dg-error "Invalid builtin arguments" } */
}
