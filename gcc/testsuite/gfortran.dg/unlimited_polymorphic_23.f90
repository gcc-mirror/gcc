! { dg-do run }
!
! Test the fix for PR65024, in which the structure for the 'info'
! component of type 'T' was not being converted into TREE_SSA and
! so caused an ICE in trans-expr.c:gfc_conv_component_ref.
!
! Reported by  <matt@gneilson.plus.com>
!
MODULE X
  TYPE T
    CLASS(*), pointer :: info
  END TYPE
END MODULE

PROGRAM P
  call bug
CONTAINS
  SUBROUTINE BUG
    USE X
    CLASS(T), pointer :: e
    integer, target :: i = 42
    allocate(e)
    e%info => NULL ()      ! used to ICE
    if (.not.associated(e%info)) e%info => i      ! used to ICE
    select type (z => e%info)
      type is (integer)
        if (z .ne.i) STOP 1
    end select
  END SUBROUTINE

  SUBROUTINE NEXT
    USE X
    CLASS (T), pointer :: e
  END SUBROUTINE
END
