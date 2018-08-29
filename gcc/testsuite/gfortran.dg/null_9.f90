! { dg-do run }

MODULE fold_convert_loc_ice
  IMPLICIT NONE
  PRIVATE

  TYPE, PUBLIC :: ta
    PRIVATE
    INTEGER :: a_comp
  END TYPE ta

  TYPE, PUBLIC :: tb
    TYPE(ta), ALLOCATABLE :: b_comp
  END TYPE tb

  PUBLIC :: proc
CONTAINS
  SUBROUTINE proc
    TYPE(tb) :: b

    b = tb(null())
    if (allocated( b%b_comp )) STOP 1
  END SUBROUTINE proc
END MODULE fold_convert_loc_ice

  USE fold_convert_loc_ice

  call proc()
END

