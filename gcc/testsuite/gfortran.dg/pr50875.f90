! { dg-do compile { target { i?86-*-* x86_64-*-* } } }
! { dg-options "-O3 -mavx" }
!
! PR fortran/50875.f90

module test

  implicit none

  integer, parameter :: dp=kind(1.d0)

  integer :: P = 2

  real(kind=dp), allocatable :: real_array_A(:),real_array_B(:,:)
  complex(kind=dp), allocatable :: cmplx_array_A(:) 

contains

  subroutine routine_A

    integer :: i

    allocate(cmplx_array_A(P),real_array_B(P,P),real_array_A(P))

    real_array_A = 1
    real_array_B = 1

    do i = 1, p
       cmplx_array_A = cmplx(real_array_B(:,i),0.0_dp,dp)
       cmplx_array_A = cmplx_array_A * exp(cmplx(0.0_dp,real_array_A+1))
    end do

    deallocate(cmplx_array_A,real_array_B,real_array_A)

  end subroutine routine_A

end module test

! { dg-final { cleanup-modules "test" } }
