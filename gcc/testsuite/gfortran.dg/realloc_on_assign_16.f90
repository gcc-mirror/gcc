! { dg-do run }
! Test the fix for PR56008
!
! Contributed by Stefan Mauerberger  <stefan.mauerberger@gmail.com>
!
PROGRAM main
    !USE MPI

    TYPE :: test_typ
        REAL, ALLOCATABLE :: a(:)
    END TYPE

    TYPE(test_typ) :: xx, yy
    TYPE(test_typ), ALLOCATABLE :: conc(:)

    !CALL MPI_INIT(i)

    xx = test_typ( [1.0,2.0] )
    yy = test_typ( [4.0,4.9] )

    conc = [ xx, yy ]

    if (any (int (10.0*conc(1)%a) .ne. [10,20])) STOP 1
    if (any (int (10.0*conc(2)%a) .ne. [40,49])) STOP 2

    !CALL MPI_FINALIZE(i)

END PROGRAM main
