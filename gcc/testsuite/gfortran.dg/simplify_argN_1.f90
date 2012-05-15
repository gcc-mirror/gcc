! { dg-do run }
! Tests the fix for PR35780, in which the assignment for C was not
! scalarized in expr.c.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
MODULE MODS
  integer, parameter :: N = 10
  INTEGER, PARAMETER, DIMENSION(N) ::  A = [(i, i = 1, N)]
  INTEGER, PARAMETER, DIMENSION(N) ::  B = [(i - 5, i = 1, N)]
  INTEGER, PARAMETER, DIMENSION(N)  :: C = ISHFTC(3, B, 5)   !ICE
  INTEGER, PARAMETER, DIMENSION(N)  :: D = ISHFTC(A, 3, 5)   !  OK
  INTEGER, PARAMETER, DIMENSION(N)  :: E = ISHFTC(A, B, 5)   !  OK

END MODULE MODS

  use mods
  integer, dimension(N) :: X = A
  integer, dimension(N) :: Y = B

! Check the simplifed expressions against the library
  if (any (ISHFTC(3, Y, 5) /= C)) call abort ()
  if (any (ISHFTC(X, 3, 5) /= D)) call abort ()
  if (any (ISHFTC(X, Y, 5) /= E)) call abort ()
end
