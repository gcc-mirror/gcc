/* Check that the option -musermode has no effect on targets that do not
   support user/privileged mode and that it does not interfere with option
   -matomic-model=soft-imask.  */
/* { dg-do compile }  */
/* { dg-options "-matomic-model=soft-imask" }  */
/* { dg-skip-if "" { "sh*-*-*" } { "*"} { "-m1*" "-m2*" } }  */

int
test (void)
{
  return 0;
}
