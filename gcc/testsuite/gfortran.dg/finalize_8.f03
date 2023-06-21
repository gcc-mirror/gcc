! { dg-do run }
!
! PR97122: Declaration of a finalizable derived type in a submodule
! IS allowed.
!
! Contributed by Ian Harvey  <ian_harvey@bigpond.com>
!
MODULE m
  IMPLICIT NONE

  INTERFACE
    MODULE SUBROUTINE other(i)
      IMPLICIT NONE
      integer, intent(inout) :: i
    END SUBROUTINE other
  END INTERFACE

  integer :: mi

END MODULE m

SUBMODULE (m) s
  IMPLICIT NONE

  TYPE :: t
    integer :: i
  CONTAINS
    FINAL :: final_t  ! Used to be an error here
  END TYPE t

CONTAINS

  SUBROUTINE final_t(arg)
    TYPE(t), INTENT(INOUT) :: arg
    mi = -arg%i
  END SUBROUTINE final_t

  module subroutine other(i)  ! 'ti' is finalized
    integer, intent(inout) :: i
    type(t) :: ti
    ti%i = i
  END subroutine other
END SUBMODULE s

  use m
  integer :: i = 42
  call other(i)
  if (mi .ne. -i) stop 1
end
