! { dg-do compile }
! PR fortran/34997
! Variable names containing $ signs
! 
      REAL*4 PLT$C_HOUSTPIX   ! { dg-error "Invalid character '\\$'" }
      INTEGER PLT$C_COMMAND   ! Unreachable as the error above is now fatal
      PARAMETER (PLT$B_OPC=0) !  Unreachable as the error above is now fatal
      common /abc$def/ PLT$C_HOUSTPIX, PLT$C_COMMAND !  Unreachable as the error above is now fatal
      end
