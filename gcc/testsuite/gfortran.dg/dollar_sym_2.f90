! { dg-do compile }
! { dg-options "-fdollar-ok" }
!
! PR fortran/34997
! Variable names containing $ signs
! 
      REAL*4 PLT$C_HOUSTPIX
      INTEGER PLT$C_COMMAND
      PARAMETER (PLT$B_OPC=0)
      common /abc$def/ PLT$C_HOUSTPIX, PLT$C_COMMAND
      end
