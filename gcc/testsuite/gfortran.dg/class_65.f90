! { dg-do run }
!
! Test the fix for PR81447 in which a vtable was not being created
! in the module 'm' so that x->vptr in 's' did not have the same
! value as that in 'p'.
!
! Contributed by Mat Cross  <mathewc@nag.co.uk>
!
Module m
  Type :: t
    integer :: i
  End Type
End Module

Program p
  Use m
  Class (t), Allocatable :: x
  Interface
    Subroutine s(x)
      Use m
      Class (t), Allocatable :: x
    End Subroutine
  End Interface
  Call s(x)
  Select Type (x)
  Type Is (t)
    Continue
  Class Is (t)
    call abort
  Class Default
    call abort
  End Select
!  Print *, 'ok'
End Program

Subroutine s(x)
  Use m, Only: t
  Implicit None
  Class (t), Allocatable :: x
  Allocate (t :: x)
End Subroutine
