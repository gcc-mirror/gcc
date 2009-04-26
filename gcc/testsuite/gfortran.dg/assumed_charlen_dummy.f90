! { dg-do compile }
! Test the fix for PR fortran/39893.
! Original testcase provided by Deji Akingunola.
! Reduced testcase provided by Dominique d'Humieres.
!
      SUBROUTINE XAUTOGET()
      CHARACTER*(*) DICBA    ! { dg-error "Entity with assumed character" }
      DATA DICBA /"CLIP" /
      RETURN
      END
