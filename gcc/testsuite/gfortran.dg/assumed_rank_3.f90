! { dg-do run }
! { dg-options "-fcheck=bounds" }
! { dg-shouldfail "Array reference out of bounds" }
!
! PR fortran/48820
!
! Do assumed-rank bound checking

implicit none
integer :: a(4,4)
call bar(a)
contains
  subroutine bar(x)
    integer :: x(..)
    print *, ubound(x,dim=3)  ! << wrong dim
  end subroutine
end

! { dg-output "Fortran runtime error: Array reference out of bounds" }
