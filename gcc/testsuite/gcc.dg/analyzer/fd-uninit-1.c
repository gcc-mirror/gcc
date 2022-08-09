/* Verify that we check for uninitialized values passed to functions
   that we have special-cased state-machine handling for.  */

int dup (int old_fd);
int not_dup (int old_fd);

int
test_1 ()
{
  int m;
  return dup (m); /* { dg-warning "use of uninitialized value 'm'" "uninit" } */
  /* { dg-bogus "'dup' on possibly invalid file descriptor 'm'" "invalid fd false +ve" { xfail *-*-* } .-1 } */
  /* XFAIL: probably covered by fix for PR analyzer/106551.  */
}

int
test_2 ()
{
  int m;
  return not_dup (m); /* { dg-warning "use of uninitialized value 'm'" } */
}
