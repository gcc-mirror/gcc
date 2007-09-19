! { dg-do compile }
! { dg-require-effective-target tls_native }

      SUBROUTINE INIT(A,B)
      REAL A, B
        COMMON /XY/ X,Y
!$OMP THREADPRIVATE (/XY/)
!$OMP SINGLE
          READ (11) A,B,X,Y
!$OMP END SINGLE COPYPRIVATE (A,B,/XY/)
      END SUBROUTINE INIT
