! { dg-do compile }
! Checks the fix for PR33664, in which the apparent function reference
! n(1) caused a seg-fault.
!
! Contributed by Henrik Holst <holst@matmech.com>
!
module test
contains
  subroutine func_1(u,n)
    integer :: n
    integer :: u(n(1))  ! { dg-error "must be PURE" }
  end subroutine
end module test
