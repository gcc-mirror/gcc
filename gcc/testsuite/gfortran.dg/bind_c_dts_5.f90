! { dg-do compile }
!
! PR fortran/50933
!
! Check whether type-compatibility checks for BIND(C) work.
!
! Contributed by Richard Maine
!

MODULE liter_cb_mod
USE ISO_C_BINDING
CONTAINS
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

END MODULE liter_cb_mod

PROGRAM main
     USE ISO_C_BINDING
   interface
   FUNCTION liter_cb(link_info) bind(C)
     USE ISO_C_BINDING
     IMPLICIT NONE
     INTEGER(c_int) liter_cb
     TYPE, bind(C) :: info_t
        INTEGER(c_int) :: type
     END TYPE info_t
     TYPE(info_t) :: link_info
   END FUNCTION liter_cb
   end interface

      TYPE, bind(C) :: info_t
        INTEGER(c_int) :: type
     END TYPE info_t  
  type(info_t) :: link_info

  write (*,*) liter_cb(link_info)

END PROGRAM main
