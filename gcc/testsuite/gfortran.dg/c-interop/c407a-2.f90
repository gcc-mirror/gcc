! { dg-do compile }
! { dg-additional-options "-fcoarray=single" }
!
! TS 29113
! C407a An assumed-type entity shall be a dummy variable that does not 
! have the ALLOCATABLE, CODIMENSION, INTENT(OUT), POINTER, or VALUE 
! attribute and is not an explicit-shape array.
!
! This test file contains tests that are expected to issue diagnostics
! for invalid code.

! Check that diagnostics are issued when type(*) is used to declare things
! that are not dummy variables.

subroutine s0 (a)
  implicit none
  integer :: a

  integer :: goodlocal
  type(*) :: badlocal  ! { dg-error "Assumed.type" }

  integer :: goodcommon
  type(*) :: badcommon  ! { dg-error "Assumed.type" }
  common /frob/ goodcommon, badcommon

  integer :: goodstatic
  type(*) :: badstatic  ! { dg-error "Assumed.type" }
  save goodstatic, badstatic

  block
    integer :: goodlocal2
    type(*) :: badlocal2  ! { dg-error "Assumed.type" }
  end block    

end subroutine

module m
  integer :: goodmodvar
  type(*) :: badmodvar ! { dg-error "Assumed.type" }
  save goodmodvar, badmodvar

  type :: t
    integer :: goodcomponent
    type(*) :: badcomponent ! { dg-error "Assumed.type" }
  end type
end module
  
! Check that diagnostics are issued when type(*) is used in combination
! with the forbidden attributes.

subroutine s1 (a) ! { dg-error "Assumed.type" }
  implicit none
  type(*), allocatable :: a
end subroutine

subroutine s2 (b) ! { dg-error "Assumed.type" }
  implicit none
  type(*), codimension[*] :: b(:,:)
end subroutine

subroutine s3 (c) ! { dg-error "Assumed.type" }
  implicit none
  type(*), intent(out) :: c
end subroutine

subroutine s4 (d) ! { dg-error "Assumed.type" }
  implicit none
  type(*), pointer :: d
end subroutine

subroutine s5 (e) ! { dg-error "Assumed.type" }
  implicit none
  type(*), value :: e
end subroutine

! Check that diagnostics are issued when type(*) is used to declare
! a dummy variable that is an explicit-shape array.

subroutine s6 (n, f) ! { dg-error "Assumed.type" }
  implicit none
  integer n
  type(*) :: f(n,n)
end subroutine

subroutine s7 (g) ! { dg-error "Assumed.type" }
  implicit none
  type(*) :: g(10)
end subroutine
