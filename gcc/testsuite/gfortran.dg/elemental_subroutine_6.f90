! { dg-do compile }
! PR35184 ICE in gfc_conv_array_index_offset
MODULE foo
  TYPE, PUBLIC :: bar
    PRIVATE
      REAL :: value
  END TYPE bar
  INTERFACE ASSIGNMENT (=)
    MODULE PROCEDURE assign_bar
  END INTERFACE ASSIGNMENT (=)
CONTAINS
  ELEMENTAL SUBROUTINE assign_bar (to, from)
    TYPE(bar), INTENT(OUT) :: to
    TYPE(bar), INTENT(IN) :: from
    to%value= from%value
  END SUBROUTINE
  SUBROUTINE my_sub (in, out)
    IMPLICIT NONE
    TYPE(bar), DIMENSION(:,:), POINTER :: in
    TYPE(bar), DIMENSION(:,:), POINTER :: out
    ALLOCATE( out(1:42, 1:42) )
    out(1, 1:42) = in(1, 1:42)
  END SUBROUTINE
END MODULE foo