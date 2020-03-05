! { dg-do compile }
! { dg-options "-O3 -fdump-tree-original -fexternal-blas" }
! PR fortran/92321 - this used to cause an ICE.  Original test case
! by Nathan Wukie.

module mod_badmatmul
    implicit none
contains

    subroutine test(c)
        real, intent(inout) :: c(3,3)
        real :: a(3,3), b(3,3)
        c = matmul(a, b)
    end subroutine test

end module mod_badmatmul

program main
    use mod_badmatmul, only: test
    implicit none

    real :: a(3,3)
    call test(a)

end program main
