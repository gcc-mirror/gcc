! { dg-do compile }
!
! PR 64508: [F03] interface check missing for procedure pointer component as actual argument
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  TYPE :: parent
  END TYPE

  TYPE, EXTENDS(parent) :: extension
    procedure(extension_proc), pointer :: ppc
  END TYPE

  CLASS(extension), ALLOCATABLE :: x
  CALL some_proc(x%ppc)               !  { dg-error "Interface mismatch in dummy procedure" }

contains

  SUBROUTINE parent_proc(arg)
    CLASS(parent), INTENT(IN) :: arg
  END SUBROUTINE

  SUBROUTINE extension_proc(arg)
    CLASS(extension), INTENT(IN) :: arg
  END SUBROUTINE


  SUBROUTINE some_proc(proc)
    PROCEDURE(parent_proc) :: proc
    TYPE(Parent) :: a
    CALL proc(a)
  END SUBROUTINE

end
