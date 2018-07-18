! { dg-do compile }
!
! Fixes of ICE on invalid & accepts invalid
!
! Contributed by Janus Weil  <janus@gcc.gnu.org>
!
implicit none

type :: param_matrix(c,r)
  integer, len :: c,r
  real :: m(c,r)
end type

type real_array(k)
  integer, kind :: k
  real(kind=k), allocatable :: r(:)
end type

type(param_matrix(1)) :: m1       ! { dg-error "does not contain enough parameter" }
type(param_matrix(1,2)) :: m2     ! ok
type(param_matrix(1,2,3)) :: m3   ! { dg-error "contains too many parameter" }
type(param_matrix(1,2.5)) :: m4   ! { dg-error "must be of INTEGER type" }

type(real_array(4)) :: a1         ! ok
type(real_array(5)) :: a2         ! { dg-error "Kind 5 not supported for type REAL" }
end
