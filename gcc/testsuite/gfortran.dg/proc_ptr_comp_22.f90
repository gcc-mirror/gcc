! { dg-do compile }
!
! PR 41978: [F03] ICE in gfc_conv_expr_descriptor for array PPC assignment
!
! Contributed by Daniel Kraft <domob@gcc.gnu.org>

MODULE m
  IMPLICIT NONE

  TYPE t
    PROCEDURE(myproc), POINTER, PASS :: myproc
  END TYPE t

CONTAINS

  INTEGER FUNCTION myproc (me)
    CLASS(t), INTENT(IN) :: me
    myproc = 42
  END FUNCTION myproc

END MODULE m

PROGRAM main
  USE m
  IMPLICIT NONE

  TYPE(t) :: arr(2)
  arr%myproc => myproc  ! { dg-error "must not have the POINTER attribute" }
END PROGRAM main
 
