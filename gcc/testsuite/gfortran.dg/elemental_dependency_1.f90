! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/35681
! Test the use of temporaries in case of elemental subroutines.

PROGRAM main
  IMPLICIT NONE
  INTEGER, PARAMETER :: sz = 5
  INTEGER :: i
  INTEGER :: a(sz) = (/ (i, i=1,sz) /)
  INTEGER :: b(sz)

  b = a
  CALL double(a(sz-b+1), a) ! { dg-warning "might interfere with actual" }
  ! Don't check the result, as the above is invalid 
  ! and might produce unexpected results (overlapping vector subscripts).


  b = a
  CALL double (a, a)               ! same range, no temporary
  IF (ANY(a /= 2*b)) CALL abort


  b = a
  CALL double (a+1, a)             ! same range, no temporary
  IF (ANY(a /= 2*b+2)) CALL abort 


  b = a
  CALL double ((a(1:sz)), a(1:sz)) ! same range, no temporary
  IF (ANY(a /= 2*b)) CALL abort


  b = a
  CALL double(a(1:sz-1), a(2:sz)) ! { dg-warning "might interfere with actual" }
  ! Don't check the result, as the above is invalid, 
  ! and might produce unexpected results (arguments overlap). 


  b = a
  CALL double((a(1:sz-1)), a(2:sz))     ! paren expression, temporary created
! { dg-final { scan-tree-dump-times "A\.17\\\[4\\\]" 1 "original" } }

  IF (ANY(a /= (/ b(1), (2*b(i), i=1,sz-1) /))) CALL abort 


  b = a
  CALL double(a(1:sz-1)+1, a(2:sz))     ! op expression, temporary created
! { dg-final { scan-tree-dump-times "A\.26\\\[4\\\]" 1 "original" } }

  IF (ANY(a /= (/ b(1), (2*b(i)+2, i=1,sz-1) /))) CALL abort 


  b = a
  CALL double(self(a), a) ! same range, no temporary
  IF (ANY(a /= 2*b)) CALL abort


  b = a
  CALL double(self(a(1:sz-1)), a(2:sz))  ! function expr, temporary created
! { dg-final { scan-tree-dump-times "A\.38\\\[4\\\]" 1 "original" } }

  IF (ANY(a /= (/ b(1), (2*b(i), i=1,sz-1) /))) CALL abort 


CONTAINS
  ELEMENTAL SUBROUTINE double(a, b)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: a
    INTEGER, INTENT(OUT) :: b
    b = 2 * a
  END SUBROUTINE double
  ELEMENTAL FUNCTION self(a)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: a
    INTEGER :: self
    self = a
  END FUNCTION self
END PROGRAM main

! { dg-final { scan-tree-dump-times "_gfortran_internal_unpack" 3 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
