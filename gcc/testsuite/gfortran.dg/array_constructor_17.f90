! { dg-do run }
! Tests the fix for PR31219, in which the character length of
! the functions in the array constructor was not being obtained
! correctly and this caused an ICE.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
  INTEGER :: J
  CHARACTER(LEN = 8) :: str
  J = 3
  write (str,'(2A4)') (/( F(I, J), I = 1, 2)/)
  IF (str .NE. " ODD EVE") call abort ()

! Comment #1 from F-X Coudert (noted by T. Burnus) that
! actually exercises a different part of the bug.
  call gee( (/g (3)/) )

CONTAINS
  FUNCTION F (K,J) RESULT(I)
    INTEGER :: K, J
    CHARACTER(LEN = J) :: I
    IF (MODULO (K, 2) .EQ. 0) THEN
       I = "EVEN"
    ELSE
       I = "ODD"
    ENDIF
  END FUNCTION

  function g(k) result(i)
    integer :: k
    character(len = k) :: i
    i = '1234'
  end function
  subroutine gee(a)
    character(*),dimension(1) :: a
    if(len (a) /= 3) call abort ()
    if(a(1) /= '123') call abort ()
  end subroutine gee

END
