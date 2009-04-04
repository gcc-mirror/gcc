! { dg-do run }
! Test the fix for PR39519, where the presence of the pointer
! as the first component was preventing the second from passing
! the "alloc_comp" attribute to the derived type.
!
! Contributed by Gilbert Scott <gilbert.scott@easynet.co.uk>
!
PROGRAM X
  TYPE T
    INTEGER, POINTER :: P
    INTEGER, ALLOCATABLE :: A(:)
  END TYPE T
  TYPE(T) :: T1,T2
  ALLOCATE ( T1%A(1) )
  ALLOCATE ( T2%A(1) )
  T1%A = 23
  T2 = T1
  T1%A = 42
  if (T2%A(1) .NE. 23) CALL ABORT
END PROGRAM X
