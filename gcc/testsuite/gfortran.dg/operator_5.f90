! { dg-do compile }
! { dg-options "-c" }

MODULE mod_t
  type :: t
    integer :: x
  end type

  ! user defined operator
  INTERFACE OPERATOR(.FOO.)
    MODULE PROCEDURE t_foo
  END INTERFACE

  INTERFACE OPERATOR(.FOO.)
    MODULE PROCEDURE t_foo                  ! { dg-error "already present" }
  END INTERFACE

  INTERFACE OPERATOR(.FOO.)
    MODULE PROCEDURE t_bar                  ! { dg-error "Ambiguous interfaces" }
  END INTERFACE

  ! intrinsic operator
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE t_foo
  END INTERFACE

  INTERFACE OPERATOR(.eq.)
    MODULE PROCEDURE t_foo                  ! { dg-error "already present" }
  END INTERFACE

  INTERFACE OPERATOR(==)
    MODULE PROCEDURE t_bar                  ! { dg-error "Ambiguous interfaces" }
  END INTERFACE

  INTERFACE OPERATOR(.eq.)
    MODULE PROCEDURE t_bar                  ! { dg-error "already present" }
  END INTERFACE

CONTAINS
  LOGICAL FUNCTION t_foo(this, other)
    TYPE(t), INTENT(in) :: this, other
    t_foo = .FALSE.
  END FUNCTION

  LOGICAL FUNCTION t_bar(this, other)
    TYPE(t), INTENT(in) :: this, other
    t_bar = .FALSE.
  END FUNCTION
END MODULE

! { dg-final { cleanup-modules "mod_t" } }
