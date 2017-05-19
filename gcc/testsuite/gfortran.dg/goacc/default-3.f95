! OpenACC default (none) clause.

subroutine f1
  implicit none
  integer :: f1_a = 2
  real, dimension (2) :: f1_b

  !$acc kernels default (none) ! { dg-message "enclosing OpenACC .kernels. construct" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .kernels. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .kernels. construct" "" { xfail *-*-* } .-1 }
  !$acc end kernels
  !$acc parallel default (none) ! { dg-message "enclosing OpenACC .parallel. construct" }
  f1_b(1) & ! { dg-error ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } }
       = f1_a; ! { dg-error ".f1_a. not specified in enclosing OpenACC .parallel. construct" }
  ! { dg-bogus ".f1_b. not specified in enclosing OpenACC .parallel. construct" "" { xfail *-*-* } .-1 }
  !$acc end parallel
end subroutine f1
