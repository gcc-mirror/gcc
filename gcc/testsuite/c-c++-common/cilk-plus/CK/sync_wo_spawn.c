/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int main (void)
{
  _Cilk_sync; /* { dg-error "expected '_Cilk_spawn' before '_Cilk_sync'" } */
  return 0;
}

