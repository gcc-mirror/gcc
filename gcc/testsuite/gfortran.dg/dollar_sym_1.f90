! { dg-do compile }
! PR fortran/34997
! Variable names containing $ signs
! 
      REAL*4 PLT$C_HOUSTPIX   ! { dg-error "Invalid character '\\$'" }
      INTEGER PLT$C_COMMAND   ! { dg-error "Invalid character '\\$'" }
      PARAMETER (PLT$B_OPC=0) ! { dg-error "Invalid character '\\$'" }
      common /abc$def/ PLT$C_HOUSTPIX, PLT$C_COMMAND ! { dg-error "Invalid character '\\$'" }
      end
