! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/57785
!
! Contributed by Kontantinos Anagnostopoulos
!
! The implicit complex conjugate was missing for DOT_PRODUCT


! For the following, the compile-time simplification fails for SUM;
! see PR fortran/56342. Hence, a manually expanded SUM is used.

!if (DOT_PRODUCT ((/ (1.0, 2.0), (2.0, 3.0) /), (/ (1.0, 1.0), (1.0, 4.0) /))   &
!   /= SUM (CONJG ((/ (1.0, 2.0), (2.0, 3.0) /))*(/ (1.0, 1.0), (1.0, 4.0) /))) &
!   STOP 1
!
!if (ANY (MATMUL ((/ (1.0, 2.0), (2.0, 3.0) /),                                 &
!                 RESHAPE ((/ (1.0, 1.0), (1.0, 4.0) /),(/2, 1/))) /=           &
!         SUM ((/ (1.0, 2.0), (2.0, 3.0) /)*(/ (1.0, 1.0), (1.0, 4.0) /))))     &
!    STOP 2      


if (DOT_PRODUCT ((/ (1.0, 2.0), (2.0, 3.0) /), (/ (1.0, 1.0), (1.0, 4.0) /))  &
    /= CONJG (cmplx(1.0, 2.0)) * cmplx(1.0, 1.0)                              &
     + CONJG (cmplx(2.0, 3.0)) * cmplx(1.0, 4.0)) &
  STOP 3

if (ANY (MATMUL ((/ (1.0, 2.0), (2.0, 3.0) /),                                &
                 RESHAPE ((/ (1.0, 1.0), (1.0, 4.0) /),(/2, 1/)))             &
         /= cmplx(1.0, 2.0) * cmplx(1.0, 1.0)                                 &
          + cmplx(2.0, 3.0) * cmplx(1.0, 4.0)))                               &
  STOP 4      
end


! { dg-final { scan-tree-dump-not "_gfortran_stop" "original" } }
