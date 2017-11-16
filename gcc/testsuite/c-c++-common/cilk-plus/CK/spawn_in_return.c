/* { dg-do compile } */
/* { dg-options "-fcilkplus -Wno-return-type" } */

int main (void)
{
  extern int foo ();
  return _Cilk_spawn foo (); /* { dg-error "return statement is not allowed" } */
}
