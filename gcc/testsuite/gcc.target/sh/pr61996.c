/* Check that the option -musermode has no effect on targets that do not
   support user/privileged mode and that it does not interfere with option
   -matomic-model=soft-imask.  */
/* { dg-do compile { target { ! has_privileged } } }  */
/* { dg-options "-matomic-model=soft-imask" }  */

int
test (void)
{
  return 0;
}
