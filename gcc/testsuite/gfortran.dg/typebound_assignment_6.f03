! { dg-do run }
!
! PR fortran/56136
! ICE on defined assignment with class arrays.
!
! Original testcase by Alipasha <alipash.celeris@gmail.com>

      MODULE A_TEST_M
        TYPE :: A_TYPE
          INTEGER :: I
          CONTAINS
          GENERIC :: ASSIGNMENT (=) => ASGN_A
          PROCEDURE, PRIVATE :: ASGN_A
        END TYPE

        CONTAINS

        ELEMENTAL SUBROUTINE ASGN_A (A, B)
          CLASS (A_TYPE), INTENT (INOUT) :: A
          CLASS (A_TYPE), INTENT (IN) :: B
          A%I = B%I
        END SUBROUTINE
      END MODULE A_TEST_M
      
      PROGRAM ASGN_REALLOC_TEST
        USE A_TEST_M
        TYPE (A_TYPE), ALLOCATABLE :: A(:)
        INTEGER :: I, J

        ALLOCATE (A(100))
        A = (/ (A_TYPE(I), I=1,SIZE(A)) /)
        A(1:50) = A(51:100)
        IF (ANY(A%I /= (/ ((50+I, I=1,SIZE(A)/2), J=1,2) /))) STOP 1
        A(::2) = A(1:50)        ! pack/unpack
        IF (ANY(A( ::2)%I /= (/ (50+I, I=1,SIZE(A)/2) /))) STOP 2
        IF (ANY(A(2::2)%I /= (/ ((50+2*I, I=1,SIZE(A)/4), J=1,2) /))) STOP 3
      END PROGRAM

