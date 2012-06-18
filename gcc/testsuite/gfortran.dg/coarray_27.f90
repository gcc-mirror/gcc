! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Coarray/coindex checks for MOVE_ALLOC
!
integer, allocatable :: a(:), b(:)[:,:], c(:)[:,:]

type t
  integer, allocatable :: d(:)
end type t
type(t) :: x[*]
class(t), allocatable :: y[:], z[:], u


call move_alloc (A, b) ! { dg-error "must have the same corank" }
call move_alloc (c, A) ! { dg-error "must have the same corank" }
call move_alloc (b, c) ! OK - same corank

call move_alloc (u, y) ! { dg-error "must have the same corank" }
call move_alloc (z, u) ! { dg-error "must have the same corank" }
call move_alloc (y, z) ! OK - same corank


call move_alloc (x%d, a)  ! OK
call move_alloc (a, x%d)  ! OK
call move_alloc (x[1]%d, a) ! { dg-error "The FROM argument to MOVE_ALLOC at .1. shall not be coindexed" }
call move_alloc (a, x[1]%d) ! { dg-error "The TO argument to MOVE_ALLOC at .1. shall not be coindexed" }

call move_alloc (y%d, a)  ! OK
call move_alloc (a, y%d)  ! OK
call move_alloc (y[1]%d, a) ! { dg-error "The FROM argument to MOVE_ALLOC at .1. shall not be coindexed" }
call move_alloc (a, y[1]%d) ! { dg-error "The TO argument to MOVE_ALLOC at .1. shall not be coindexed" }

end
