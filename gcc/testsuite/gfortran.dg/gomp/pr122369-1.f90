! { dg-do compile }
! { dg-additional-options "-Wunused-label" }

! Check that a format label referenced in the first statement past a
! metadirective body is bound to the outer region.

!$omp  metadirective when(user={condition(.true.)}: target teams  &
!$omp&        distribute parallel do)
      DO JCHECK = 1, MNMIN
      END DO
      WRITE(6,366) PCHECK, UCHECK, VCHECK
 366  FORMAT(/, ' Vcheck = ',E12.4,/)
      END PROGRAM
