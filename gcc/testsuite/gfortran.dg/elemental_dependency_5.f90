! { dg-do run }
! { dg-require-visibility "" }
!
! Tests the fix for PR64952.
!
! Original report by Nick Maclaren  <nmm1@cam.ac.uk> on clf
! https://groups.google.com/forum/#!topic/comp.lang.fortran/TvVY5j3GPmg
! See elemental_dependency_4.f90
!
! This test contributed by Mikael Morin  <mikael.morin@sfr.fr>
!
MODULE M
    INTEGER, PRIVATE :: i

    TYPE, ABSTRACT :: t
      REAL :: f
    CONTAINS
      PROCEDURE(Fred_ifc), DEFERRED, PASS :: tbp
    END TYPE t
    TYPE, EXTENDS(t) :: t2
    CONTAINS
      PROCEDURE :: tbp => Fred
    END TYPE t2

    TYPE(t2) :: array(5) = (/ (t2(i+0.0), i = 1,5) /)

    INTERFACE
        ELEMENTAL FUNCTION Fred_ifc (x, n)
            IMPORT
            REAL :: Fred
            CLASS(T), INTENT(IN) :: x
            INTEGER, INTENT(IN) :: n
        END FUNCTION Fred_ifc
    END INTERFACE

CONTAINS
    ELEMENTAL FUNCTION Fred (x, n)
        REAL :: Fred
        CLASS(T2), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: n
        Fred = x%f+SUM(array(:n-1)%f)+SUM(array(n+1:)%f)
     END FUNCTION Fred
END MODULE M

PROGRAM Main
    USE M
    INTEGER :: i, index(5) = (/ (i, i = 1,5) /)
    
    array%f = array%tbp(index)
    if (any (array%f .ne. array(1)%f)) call abort

    array%f = index
    call Jack(array)
  CONTAINS
    SUBROUTINE Jack(dummy)
        CLASS(t) :: dummy(:)
        dummy%f = dummy%tbp(index)
        !print *, dummy%f
        if (any (dummy%f .ne. 15.0)) call abort
    END SUBROUTINE
END PROGRAM Main

