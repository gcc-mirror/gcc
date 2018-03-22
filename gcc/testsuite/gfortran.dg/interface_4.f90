! { dg-do run }
! Tests the fix for the interface bit of PR29975, in which the
! interfaces bl_copy were rejected as ambiguous, even though
! they import different specific interfaces.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk> and
! simplified by Tobias Burnus <burnus@gcc.gnu.org>
!
SUBROUTINE RECOPY(N, c)
  real, INTENT(IN) :: N
  character(6) :: c
  c = "recopy"
END SUBROUTINE RECOPY

MODULE f77_blas_extra
PUBLIC :: BL_COPY
INTERFACE BL_COPY
  MODULE PROCEDURE SDCOPY
END INTERFACE BL_COPY
CONTAINS
   SUBROUTINE SDCOPY(N, c)
    INTEGER, INTENT(IN) :: N
    character(6) :: c
    c = "sdcopy"
   END SUBROUTINE SDCOPY
END MODULE f77_blas_extra

MODULE f77_blas_generic
INTERFACE BL_COPY
   SUBROUTINE RECOPY(N, c)
    real, INTENT(IN) :: N
    character(6) :: c
   END SUBROUTINE RECOPY
END INTERFACE BL_COPY
END MODULE f77_blas_generic

program main
  USE f77_blas_extra
  USE f77_blas_generic
  character(6) :: chr
  call bl_copy(1, chr)
  if (chr /= "sdcopy") STOP 1
  call bl_copy(1.0, chr)
  if (chr /= "recopy") STOP 2  
end program main
