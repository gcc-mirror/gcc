! { dg-do compile }
! PR71649 Internal Compiler Error
SUBROUTINE Compiler_Options ( Options, Version, WriteOpt )
   USE ISO_FORTRAN_ENV, ONLY : Compiler_Version, Compiler_Options ! { dg-error "already declared" }
   IMPLICIT NONE
   CHARACTER (LEN=*), INTENT(OUT) :: Options
   CHARACTER (LEN=*), INTENT(OUT) :: Version
   LOGICAL, INTENT(IN), OPTIONAL  :: WriteOpt
   Version = Compiler_Version()
   Options = Compiler_Options() ! { dg-error "Unexpected use of subroutine name" }
   RETURN
END SUBROUTINE Compiler_Options

