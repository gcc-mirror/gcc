! { dg-do compile }
! { dg-additional-options "-Wall" }

! PR fortran/122570

      SUBROUTINE INITAL
      implicit none (type, external)
      integer :: j, n
      n = 5
!$omp  metadirective                                                            &
!$omp&    when(user={condition(.true.)}: target teams                           &
!$omp&        distribute parallel do)                                           &
!$omp&    when(user={condition(.false.)}: target teams                          &
!$omp&        distribute parallel do) 
      DO J=1,N
      END DO
      END SUBROUTINE

      SUBROUTINE CALC3
       implicit none (type, external)
       integer :: i, m
       m = 99
!$omp  metadirective 
!$omp& when(user={condition(.false.)}:
!$omp&      simd)               
      DO 301 I=1,M
  301 CONTINUE
  300 CONTINUE ! { dg-warning "Label 300 at .1. defined but not used \\\[-Wunused-label\\\]" }
      END SUBROUTINE
