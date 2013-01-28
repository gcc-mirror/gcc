! { dg-do compile }
!
! PR fortran/53537
! The definition of T1 in the interface used to be rejected because T3
! was imported under the original name T1.

       MODULE MOD
         TYPE T1
           SEQUENCE
           integer :: j
         END TYPE t1
       END
       PROGRAM MAIN
         USE MOD, T3 => T1
         INTERFACE SUBR
           SUBROUTINE SUBR1(X,y)
             IMPORT :: T3
             type t1
!               sequence
!               integer :: i
             end type t1
             TYPE(T3) X
!             TYPE(T1) X
           END SUBROUTINE
         END INTERFACE SUBR
       END PROGRAM MAIN


