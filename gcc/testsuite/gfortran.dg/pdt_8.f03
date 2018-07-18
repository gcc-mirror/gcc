! { dg-do compile }
!
! Fixes of "accepts invalid".
! Note that the undeclared parameter 'y' in 't1' was originally in the
! type 't'. It turned out to be convenient to defer the error until the
! type is used in the declaration of 'z'.
!
! Contributed by Janus Weil  <janus@gcc.gnu.org>
!
implicit none
type :: t(i,a,x)         ! { dg-error "does not|has neither" }
  integer, kind :: k     ! { dg-error "does not not appear in the type parameter list" }
  integer :: i           ! { dg-error "has neither the KIND nor LEN attribute" }
  integer, kind :: a(3)  ! { dg-error "must be a scalar" }
  real, kind :: x        ! { dg-error "must be INTEGER" }
end type

type :: t1(k,y)          ! { dg-error "does not have a component" }
  integer, kind :: k
end type

! This is a knock-on from the previous error
type(t1(4,4)) :: z       ! { dg-error "Invalid character in name" }
end
