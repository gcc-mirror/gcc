/* { dg-options "-fcilkplus" } */

extern int foo ();
int bar = _Cilk_spawn foo (); /* { dg-error "may only be used inside a function" } */


int main (void)
{
  int x; 

  _Cilk_spawn foo; /* { dg-error "only function calls can be spawned" } */
  _Cilk_spawn x; /* { dg-error "only function calls can be spawned" } */
  return x;
}
