! { dg-do run }
! PR 49479 - this used not to print anything.
! Test case by Joost VandeVondele.
MODULE M1
  IMPLICIT NONE
  type foo
     character(len=5) :: x
  end type foo
CONTAINS
  SUBROUTINE S1(data)
    INTEGER, DIMENSION(:), INTENT(IN), &
         OPTIONAL                               :: DATA
    character(20) :: line
    IF (.not. PRESENT(data)) call abort
    write (unit=line,fmt='(I5)') size(data)
    if (line /= '    0               ') call abort
  END SUBROUTINE S1

  subroutine s_type(data)
    type(foo), dimension(:), intent(in), optional :: data
    character(20) :: line
    IF (.not. PRESENT(data)) call abort
    write (unit=line,fmt='(I5)') size(data)
    if (line /= '    0               ') call abort
  end subroutine s_type

  SUBROUTINE S2(N)
    INTEGER :: N
    INTEGER, ALLOCATABLE, DIMENSION(:, :)    :: blki
    type(foo), allocatable, dimension(:, :)  :: bar
    ALLOCATE(blki(3,N))
    allocate (bar(3,n))
    blki=0
    CALL S1(RESHAPE(blki,(/3*N/)))
    call s_type(reshape(bar, (/3*N/)))
  END SUBROUTINE S2

END MODULE M1

USE M1
CALL S2(0)
END
