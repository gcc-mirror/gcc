/* { dg-do assemble  } */
/* { dg-options "-Werror=return-local-addr" } */
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */

int* bad()
{
  int x = 0;
  return &x;		/* { dg-error "address of local variable" } */
}
