! { dg-do compile }
!
! PR fortran/44742
!
! Test case based on Juergen Reuter's and reduced by
! Janus Weil.
!
! The program creates a large array constructor, which
! exceeds -fmax-array-constructor - and caused an ICE.
!

module proc8
  implicit none
  integer, parameter :: N = 256
  logical, dimension(N**2), parameter :: A = .false.
  logical, dimension(N,N), parameter :: B &
    = reshape ( (/ A /), (/ N, N /) ) ! { dg-error "array constructor at .1. requires an increase" }
end module
