! { dg-do compile }
!
! PR 57966: [OOP] Using a TBP to specify the shape of a dummy argument
!
! Contributed by Stefan Mauerberger <stefan.mauerberger@gmail.com>

MODULE my_mod
  IMPLICIT NONE

  TYPE config_cls
  CONTAINS
    PROCEDURE, NOPASS :: my_size
    PROCEDURE, NOPASS :: my_sub
    GENERIC :: sz => my_size
    GENERIC :: sub => my_sub
  END TYPE

  TYPE(config_cls) :: config

CONTAINS

  PURE INTEGER FUNCTION my_size()
    my_size = 10
  END FUNCTION
  
  SUBROUTINE my_sub
  END SUBROUTINE
  
  SUBROUTINE test (field1, field2, field3, field4)
    REAL :: field1 (config%my_size())
    REAL :: field2 (config%sz())
    REAL :: field3 (config%my_sub())  ! { dg-error "should be a FUNCTION" }
    REAL :: field4 (config%sub())     ! { dg-error "should be a FUNCTION" }
  END SUBROUTINE

END MODULE

! { dg-final { cleanup-modules "my_mod" } }
