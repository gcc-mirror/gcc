! { dg-do run }
!
! PR fortran/19107
! -fwhole-file flag added for PR fortran/44945
!
! This test the fix of PR19107, where character array actual
! arguments in derived type constructors caused an ICE.
! It also checks that the scalar counterparts are OK.
! Contributed by Paul Thomas  pault@gcc.gnu.org
!
MODULE global
  TYPE :: dt
    CHARACTER(4) a
    CHARACTER(4) b(2)
  END TYPE
  TYPE (dt), DIMENSION(:), ALLOCATABLE, SAVE :: c
END MODULE global
program char_array_structure_constructor
  USE global
  call alloc (2)
  if ((any (c%a /= "wxyz")) .OR. &
      (any (c%b(1) /= "abcd")) .OR. &
      (any (c%b(2) /= "efgh"))) STOP 1
contains
  SUBROUTINE alloc (n)
    USE global
    ALLOCATE (c(n), STAT=IALLOC_FLAG)
    DO i = 1,n
      c (i) = dt ("wxyz",(/"abcd","efgh"/))
    ENDDO
  end subroutine alloc
END program char_array_structure_constructor
