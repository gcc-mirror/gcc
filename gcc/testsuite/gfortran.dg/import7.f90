! { dg-do compile }
!
! PR39688: IMPORT of derived type fails
!
! Contributed by Bob Corbett <robert.corbett@sun.com>

       MODULE MOD
         TYPE T1
           SEQUENCE
           TYPE(T2), POINTER :: P
         END TYPE
         TYPE T2
           SEQUENCE
           INTEGER I
         END TYPE
       END

       PROGRAM MAIN
         USE MOD, T3 => T1, T4 => T2
         TYPE T1
           SEQUENCE
           TYPE(T2), POINTER :: P
         END TYPE
         INTERFACE SUBR
           SUBROUTINE SUBR1(X)
             IMPORT T3
             TYPE(T3) X
           END SUBROUTINE
           SUBROUTINE SUBR2(X)
             IMPORT T1
             TYPE(T1) X
           END SUBROUTINE
         END INTERFACE
         TYPE T2
           SEQUENCE
           REAL X
         END TYPE
       END

       SUBROUTINE SUBR1(X)
         USE MOD
         TYPE(T1) X
       END

       SUBROUTINE SUBR2(X)
         TYPE T1
           SEQUENCE
           TYPE(T2), POINTER :: P
         END TYPE
         TYPE T2
           SEQUENCE
           REAL X
         END TYPE
         TYPE(T1) X
       END
