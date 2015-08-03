! { dg-do run }
!
! PR fortran/64921
! Test that the finalization wrapper procedure get the always_explicit
! attribute so that the array is not passed without descriptor from 
! T3's finalization wrapper procedure to T2's one.
!
! Contributed by Mat Cross  <mathewc@nag.co.uk>

Program test
  Implicit None
  Type :: t1
    Integer, Allocatable :: i
  End Type
  Type :: t2
    Integer, Allocatable :: i
  End Type
  Type, Extends (t1) :: t3
    Type (t2) :: j
  End Type
  Type, Extends (t3) :: t4
    Integer, Allocatable :: k
  End Type
  Call s
  Print *, 'ok'
Contains
  Subroutine s
    Class (t1), Allocatable :: x
    Allocate (t4 :: x)
  End Subroutine
End Program
! { dg-output "ok" }
