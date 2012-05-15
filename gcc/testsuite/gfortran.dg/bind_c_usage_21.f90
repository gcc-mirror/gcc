! { dg-do compile }
!
! PR fortran/45211
!
! Contributed by Scot Breitenfeld
!
module m
contains
  FUNCTION liter_cb(link_info) bind(C)
    USE ISO_C_BINDING
    IMPLICIT NONE

    INTEGER(c_int) liter_cb

    TYPE, bind(C) :: info_t
       INTEGER(c_int) :: type
    END TYPE info_t

    TYPE(info_t) :: link_info

    liter_cb = 0
  END FUNCTION liter_cb
end module m
