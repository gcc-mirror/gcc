! { dg-do run }
!
! PR 64230: [4.9/5 Regression] Invalid memory reference in a compiler-generated finalizer for allocatable component
!
! Contributed by Mat Cross <mathewc@nag.co.uk>

Program main
  Implicit None
  Type :: t1
  End Type
  Type, Extends (t1) :: t2
    Integer, Allocatable :: i
  End Type
  Type, Extends (t2) :: t3
    Integer, Allocatable :: j
  End Type
  Class (t1), Allocatable :: t
  Allocate (t3 :: t)
  print *,"allocated!"
  Deallocate (t)
End
