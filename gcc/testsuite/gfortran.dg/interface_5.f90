! { dg-do compile }
! Tests the fix for the interface bit of PR29975, in which the
! interfaces bl_copy were rejected as ambiguous, even though
! they import different specific interfaces.  In this testcase,
! it is verified that ambiguous specific interfaces are caught.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk> and
! simplified by Tobias Burnus <burnus@gcc.gnu.org>
!
SUBROUTINE RECOPY(N, c)
  real, INTENT(IN) :: N
  character(6) :: c
  print *, n
  c = "recopy"
END SUBROUTINE RECOPY

MODULE f77_blas_extra
PUBLIC :: BL_COPY
INTERFACE BL_COPY
  MODULE PROCEDURE SDCOPY
END INTERFACE BL_COPY
CONTAINS
   SUBROUTINE SDCOPY(N, c)
    REAL, INTENT(IN) :: N
    character(6) :: c
    print *, n
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

subroutine i_am_ok
  USE f77_blas_extra ! { dg-warning "ambiguous interfaces" }
  USE f77_blas_generic
  character(6) :: chr
  chr = ""
  if (chr /= "recopy") call abort ()  
end subroutine i_am_ok

program main
  USE f77_blas_extra ! { dg-error "Ambiguous interfaces" }
  USE f77_blas_generic
  character(6) :: chr
  chr = ""
  call bl_copy(1.0, chr)
  if (chr /= "recopy") call abort ()  
end program main
! { dg-final { cleanup-modules "f77_blas_generic f77_blas_extra" } }
