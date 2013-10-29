/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int main (void)
{
  extern int foo ();
  return _Cilk_spawn foo (); /* { dg-error "return statement is not allowed" } */
}
