! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
! { dg-require-visibility "" }
!
! Tests the fix for PR64952, in which the assignment to 'array' should
! have generated a temporary because of the references to the lhs in
! the function 'Fred'.
!
! Original report, involving function 'Nick'
! Contributed by Nick Maclaren  <nmm1@cam.ac.uk> on clf
! https://groups.google.com/forum/#!topic/comp.lang.fortran/TvVY5j3GPmg
!
! Other tests are due to Mikael Morin  <mikael.morin@sfr.fr>
!
MODULE M
    INTEGER, PRIVATE :: i
    REAL :: arraym(5) = (/ (i+0.0, i = 1,5) /)
CONTAINS
    ELEMENTAL FUNCTION Bill (n, x)
        REAL :: Bill
        INTEGER, INTENT(IN) :: n
        REAL, INTENT(IN) :: x
        Bill = x+SUM(arraym(:n-1))+SUM(arraym(n+1:))
    END FUNCTION Bill

    ELEMENTAL FUNCTION Charles (x)
        REAL :: Charles
        REAL, INTENT(IN) :: x
        Charles = x
    END FUNCTION Charles
END MODULE M

ELEMENTAL FUNCTION Peter(n, x)
    USE M
    REAL :: Peter
    INTEGER, INTENT(IN) :: n
    REAL, INTENT(IN) :: x
    Peter = Bill(n, x)
END FUNCTION Peter

PROGRAM Main
    use M
    INTEGER :: i, index(5) = (/ (i, i = 1,5) /)
    REAL :: array(5) = (/ (i+0.0, i = 1,5) /)

    INTERFACE
        ELEMENTAL FUNCTION Peter(n, x)
            REAL :: Peter
            INTEGER, INTENT(IN) :: n
            REAL, INTENT(IN) :: x
        END FUNCTION Peter
    END INTERFACE

    PROCEDURE(Robert2), POINTER :: missme => Null()

    ! Original testcase
    array = Nick(index,array)
    If (any (array .ne. array(1))) STOP 1

    array = (/ (i+0.0, i = 1,5) /)
    ! This should not create a temporary
    array = Charles(array)
    If (any (array .ne. index)) STOP 2
    ! { dg-final { scan-tree-dump-times "array\\\[\[^\\\]\]*\\\]\\s*=\\s*charles\\s*\\(&array\\\[\[^\\\]\]*\\\]\\);" 1 "original" } }

    ! Check use association of the function works correctly.
    arraym = Bill(index,arraym)
    if (any (arraym .ne. arraym(1))) STOP 3

    ! Check siblings interact correctly.
    array = (/ (i+0.0, i = 1,5) /)
    array = Henry(index)
    if (any (array .ne. array(1))) STOP 4

    array = (/ (i+0.0, i = 1,5) /)
    ! This should not create a temporary
    array = index + Henry2(0) - array
    ! { dg-final { scan-tree-dump-times "array\\\[\[^\\\]\]*\\\]\\s*=\\s*\\(\\(real\\(kind=4\\)\\)\\s*index\\\[\[^\\\]\]*\\\]\\s*\\+\\s*D.\\d*\\)\\s*-\\s*array\\\[\[^\\\]\]*\\\];" 1 "original" } }
    if (any (array .ne. 15.0)) STOP 5

    arraym = (/ (i+0.0, i = 1,5) /)
    arraym = Peter(index, arraym)
    if (any (arraym .ne. 15.0)) STOP 6

    array = (/ (i+0.0, i = 1,5) /)
    array = Robert(index)
    if (any (arraym .ne. 15.0)) STOP 7

    missme => Robert2
    array = (/ (i+0.0, i = 1,5) /)
    array = David(index)
    if (any (arraym .ne. 15.0)) STOP 8

    array = (/ (i+0.0, i = 1,5) /)
    array = James(index)
    if (any (arraym .ne. 15.0)) STOP 9

    array = (/ (i+0.0, i = 1,5) /)
    array = Romeo(index)
    if (any (arraym .ne. 15.0)) STOP 10

CONTAINS
    ELEMENTAL FUNCTION Nick (n, x)
        REAL :: Nick
        INTEGER, INTENT(IN) :: n
        REAL, INTENT(IN) :: x
        Nick = x+SUM(array(:n-1))+SUM(array(n+1:))
    END FUNCTION Nick

! Note that the inverse order of Henry and Henry2 is trivial.
! This way round, Henry2 has to be resolved before Henry can
! be marked as having an inherited external array reference.
    ELEMENTAL FUNCTION Henry2 (n)
        REAL :: Henry2
        INTEGER, INTENT(IN) :: n
        Henry2 = n + SUM(array(:n-1))+SUM(array(n+1:))
    END FUNCTION Henry2

    ELEMENTAL FUNCTION Henry (n)
        REAL :: Henry
        INTEGER, INTENT(IN) :: n
        Henry = Henry2(n)
    END FUNCTION Henry

    PURE FUNCTION Robert2(n)
        REAL :: Robert2
        INTEGER, INTENT(IN) :: n
        Robert2 = Henry(n)
    END FUNCTION Robert2

    ELEMENTAL FUNCTION Robert(n)
        REAL :: Robert
        INTEGER, INTENT(IN) :: n
        Robert = Robert2(n)
    END FUNCTION Robert

    ELEMENTAL FUNCTION David (n)
        REAL :: David
        INTEGER, INTENT(IN) :: n
        David = missme(n)
    END FUNCTION David

    ELEMENTAL SUBROUTINE James2 (o, i)
        REAL, INTENT(OUT) :: o
        INTEGER, INTENT(IN) :: i
        o = Henry(i)
    END SUBROUTINE James2

    ELEMENTAL FUNCTION James(n)
        REAL :: James
        INTEGER, INTENT(IN) :: n
        CALL James2(James, n)
    END FUNCTION James

    FUNCTION Romeo2(n)
        REAL :: Romeo2
        INTEGER, INTENT(in) :: n
        Romeo2 = Henry(n)
    END FUNCTION Romeo2

    IMPURE ELEMENTAL FUNCTION Romeo(n)
        REAL :: Romeo
        INTEGER, INTENT(IN) :: n
        Romeo = Romeo2(n)
    END FUNCTION Romeo
END PROGRAM Main

