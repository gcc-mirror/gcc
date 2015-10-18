! { dg-do compile }
! PR fortran/67758
!
! Check the absence of ICE after emitting the error message
!
! This test is  the free form variant of common_24.f.

      REAL :: X
      COMMON /FMCOM / X(80 000 000)  ! { dg-error "Expected another dimension" }
      CALL T(XX(A))
      COMMON /FMCOM / XX(80 000 000) ! { dg-error "Expected another dimension" }
      END
