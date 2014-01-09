/* { dg-do compile } */
/* { dg-options " " } */  

/* Tests the errors when Cilk keywords are used without -fcilkplus.  */

void foo()
{
    _Cilk_spawn foo(); /* { dg-error "must be enabled to use" } */
}

void foo2 ()
{
  _Cilk_spawn foo (); /* { dg-error "must be enabled to use" } */
  _Cilk_sync; /* { dg-error "must be enabled to use" } */
}
