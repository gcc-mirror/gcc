! OpenACC default (none) clause.

subroutine f1
  implicit none
  integer :: f1_a = 2
  real, dimension (2) :: f1_b

  !$acc kernels default (none) ! { dg-note "enclosing OpenACC .kernels. construct with 'default\\\(none\\\)' clause" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } .-1 }
  !$acc end kernels
  !$acc parallel default (none) ! { dg-note "enclosing OpenACC .parallel. construct with 'default\\\(none\\\)' clause" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } .-1 }
  !$acc end parallel
  !$acc serial default (none) ! { dg-note "enclosing OpenACC .serial. construct with 'default\\\(none\\\)' clause" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } .-1 }
  !$acc end serial

  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc kernels ! { dg-note "enclosing OpenACC 'kernels' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } .-1 }
  !$acc end kernels
  !$acc end data
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc parallel ! { dg-note "enclosing OpenACC 'parallel' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } .-1 }
  !$acc end parallel
  !$acc end data
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc serial ! { dg-note "enclosing OpenACC 'serial' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } .-1 }
  !$acc end serial
  !$acc end data

  !$acc data default (none)
  !$acc kernels default (none) ! { dg-note "enclosing OpenACC .kernels. construct with 'default\\\(none\\\)' clause" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } .-1 }
  !$acc end kernels
  !$acc end data
  !$acc data default (none)
  !$acc parallel default (none) ! { dg-note "enclosing OpenACC .parallel. construct with 'default\\\(none\\\)' clause" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } .-1 }
  !$acc end parallel
  !$acc end data
  !$acc data default (none)
  !$acc serial default (none) ! { dg-note "enclosing OpenACC .serial. construct with 'default\\\(none\\\)' clause" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } .-1 }
  !$acc end serial
  !$acc end data

  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc data
  !$acc data
  !$acc kernels ! { dg-note "enclosing OpenACC 'kernels' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } .-1 }
  !$acc end kernels
  !$acc end data
  !$acc end data
  !$acc end data
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc data
  !$acc data
  !$acc parallel ! { dg-note "enclosing OpenACC 'parallel' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } .-1 }
  !$acc end parallel
  !$acc end data
  !$acc end data
  !$acc end data
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc data
  !$acc data
  !$acc serial ! { dg-note "enclosing OpenACC 'serial' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } .-1 }
  !$acc end serial
  !$acc end data
  !$acc end data
  !$acc end data

  !$acc data
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc data
  !$acc kernels ! { dg-note "enclosing OpenACC 'kernels' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } .-1 }
  !$acc end kernels
  !$acc end data
  !$acc end data
  !$acc end data
  !$acc data
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc data
  !$acc parallel ! { dg-note "enclosing OpenACC 'parallel' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } .-1 }
  !$acc end parallel
  !$acc end data
  !$acc end data
  !$acc end data
  !$acc data
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc data
  !$acc serial ! { dg-note "enclosing OpenACC 'serial' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } .-1 }
  !$acc end serial
  !$acc end data
  !$acc end data
  !$acc end data

  !$acc data
  !$acc data
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc kernels ! { dg-note "enclosing OpenACC 'kernels' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } .-1 }
  !$acc end kernels
  !$acc end data
  !$acc end data
  !$acc end data
  !$acc data
  !$acc data
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc parallel ! { dg-note "enclosing OpenACC 'parallel' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } .-1 }
  !$acc end parallel
  !$acc end data
  !$acc end data
  !$acc end data
  !$acc data
  !$acc data
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc serial ! { dg-note "enclosing OpenACC 'serial' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } .-1 }
  !$acc end serial
  !$acc end data
  !$acc end data
  !$acc end data

  !$acc data
  !$acc data default (none)
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc kernels ! { dg-note "enclosing OpenACC 'kernels' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } .-1 }
  !$acc end kernels
  !$acc end data
  !$acc end data
  !$acc end data
  !$acc data
  !$acc data default (none)
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc parallel ! { dg-note "enclosing OpenACC 'parallel' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } .-1 }
  !$acc end parallel
  !$acc end data
  !$acc end data
  !$acc end data
  !$acc data
  !$acc data default (none)
  !$acc data default (none) ! { dg-note "enclosing OpenACC 'data' construct with 'default\\\(none\\\)' clause" }
  !$acc serial ! { dg-note "enclosing OpenACC 'serial' construct and" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .serial. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .serial. construct" "" { xfail *-*-* } .-1 }
  !$acc end serial
  !$acc end data
  !$acc end data
  !$acc end data

end subroutine f1
