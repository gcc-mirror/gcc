! { dg-do compile }
!
! Test the fix for the following:
! PR fortran/93483
! PR fortran/107216
! PR fortran/107219
!
! Contributed by G.Steinmetz

program p
  real, parameter :: r0(*) = +[real :: +(1) ]
  real, parameter :: r1(*) = +[real :: +[1] ]
  real, parameter :: r2(*) = -[real :: [(1)]]
  real, parameter :: r3(*) = +[real :: [-(1)]]
  real, parameter :: r4(*) = -[real :: [[(1)]]]
  real, parameter :: r5(*) = -[real :: -[1, 2]]
  real, parameter :: r6(*) = +[real :: +[1, 2]]
  real, parameter :: r7(*) =  [real :: 1, 2] * [real :: 1, (2)]
  real, parameter :: r8(*) =  [real :: 1, (2)] * [real :: 1, 2]
  real, parameter :: r9(*) = +[real :: 1, 2] * [real :: 1, (2)]
  real, parameter :: rr(*) = -[real :: 1, (2)] * [real :: 1, 2]
end
