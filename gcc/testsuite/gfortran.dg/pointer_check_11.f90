! { dg-do run }
! { dg-options "-fcheck=all" }
!
! { dg-shouldfail "Pointer check" }
! { dg-output "Fortran runtime error: Pointer actual argument 'y' is not associated" }
!
!
! PR fortran/50718
!
! Was failing (ICE) with -fcheck=pointer if the dummy had the value attribute.

type t
  integer :: p
end type t

type(t), pointer :: y => null()

call sub(y) ! Invalid: Nonassociated pointer

contains
  subroutine sub (x)
    type(t), value :: x
  end subroutine
end
