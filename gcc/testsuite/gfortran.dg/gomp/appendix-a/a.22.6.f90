! { dg-do compile }
! { dg-require-effective-target tls_native }

       SUBROUTINE A22_6_GOOD()
             COMMON /T/ A
!$OMP THREADPRIVATE(/T/)
       CONTAINS
         SUBROUTINE A22_6S_GOOD()
           COMMON /T/ A
!$OMP THREADPRIVATE(/T/)
!$OMP PARALLEL COPYIN(/T/)
!$OMP END PARALLEL
        END SUBROUTINE A22_6S_GOOD
      END SUBROUTINE A22_6_GOOD
