! { dg-do compile }
! PR 60355 - there was no error message for implicitly typed variables
! Test case contributed by Vladimir Fuka
program main
  bind(c) test_BIND ! { dg-error "cannot be BIND" }
END
