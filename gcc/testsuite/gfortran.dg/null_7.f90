! { dg-do compile }
!
! PR fortran/55763
!

implicit none
integer, pointer :: x
class(*), pointer :: y
integer, pointer :: p1 => null(x) ! { dg-error "NULL.. initialization at .1. may not have MOLD" }
integer, pointer :: p2 => null(mold=x) ! { dg-error "NULL.. initialization at .1. may not have MOLD" }
class(*), pointer :: p3 =>null(x) ! { dg-error "NULL.. initialization at .1. may not have MOLD" }
type t
  real, pointer :: a1 => null(x) ! { dg-error "NULL.. initialization at .1. may not have MOLD" }
  real, pointer :: a2 => null ( mold = x) ! { dg-error "NULL.. initialization at .1. may not have MOLD" }
  class(*), pointer :: a3 => null(mold = x )  ! { dg-error "NULL.. initialization at .1. may not have MOLD" }
end type t

x => null(x) ! OK
y => null(y) ! OK
end
