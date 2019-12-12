! { dg-do compile }
! PR71649 Internal Compiler Error
SUBROUTINE Compiler_Options ( Options, Version, WriteOpt )        ! { dg-error "\(1\)" }
   USE ISO_FORTRAN_ENV, ONLY : Compiler_Version, Compiler_Options ! { dg-error "conflicts with the" }
   IMPLICIT NONE
   CHARACTER (LEN=*), INTENT(OUT) :: Options
   CHARACTER (LEN=*), INTENT(OUT) :: Version
   LOGICAL, INTENT(IN), OPTIONAL  :: WriteOpt
   Version = Compiler_Version()  ! { dg-error "has no IMPLICIT type" }
   Options = Compiler_Options()  ! { dg-error "Unexpected use of subroutine name" }
   RETURN
END SUBROUTINE Compiler_Options

