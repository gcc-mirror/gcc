! { dg-do run }
! { dg-additional-options "-std=f2008" }
!
! PR fortran/68020
!
! Contributed by Gerhard Steinmetz
!
subroutine rank_1_2
  integer, parameter :: a(1, 2) = 0
  integer, parameter :: x(*, *) = a
  integer, parameter :: y(11:*, 12:*) = a
  integer :: k
  if (any (lbound(x) /= [1,1])) stop 1
  if (any (ubound(x) /= [1,2])) stop 2
  if (any (lbound(y) /= [11,12])) stop 3
  if (any (ubound(y) /= [11,13])) stop 4
end

subroutine rank_3
  integer, parameter :: a(1, 2, 3) = 0
  integer, parameter :: x(*, *, *) = a
  integer, parameter :: y(11:*, 12:*, 13:*) = a
  integer :: k
  if (any (lbound(x) /= [1,1,1])) stop 5
  if (any (ubound(x) /= [1,2,3])) stop 6
  if (any (lbound(y) /= [11,12,13])) stop 7
  if (any (ubound(y) /= [11,13,15])) stop 8
end

subroutine rank_4
  integer, parameter :: a(1, 2, 3, 4) = 0
  integer, parameter :: x(*, *, *, *) = a
  integer, parameter :: y(11:*, 12:*, 13:*, 14:*) = a
  integer :: k
  if (any (lbound(x) /= [1,1,1,1])) stop 9
  if (any (ubound(x) /= [1,2,3,4])) stop 10
  if (any (lbound(y) /= [11,12,13,14])) stop 11
  if (any (ubound(y) /= [11,13,15,17])) stop 12
end

program p
  call rank_1_2
  call rank_3
  call rank_4
end program p
