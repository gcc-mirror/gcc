! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR36932 and PR36933, in which unnecessary
! temporaries were being generated.  The module m2 tests the
! additional testcase in comment #3 of PR36932.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
MODULE M2
  IMPLICIT NONE
  TYPE particle
   REAL :: r(3)
  END TYPE
CONTAINS
  SUBROUTINE S1(p)
     TYPE(particle), POINTER, DIMENSION(:) :: p
     REAL :: b(3)
     INTEGER :: i
     b=pbc(p(i)%r)
  END SUBROUTINE S1
  FUNCTION pbc(b)
     REAL :: b(3)
     REAL :: pbc(3)
     pbc=b
  END FUNCTION
END MODULE M2

MODULE M1
  IMPLICIT NONE
  TYPE cell_type
     REAL :: h(3,3)
  END TYPE
CONTAINS
  SUBROUTINE S1(cell)
     TYPE(cell_type), POINTER :: cell
     REAL :: a(3)
     REAL :: b(3) = [1, 2, 3]
     a=MATMUL(cell%h,b)
     if (ANY (INT (a) .ne. [30, 36, 42])) call abort
  END SUBROUTINE S1
END MODULE M1

  use M1
  TYPE(cell_type), POINTER :: cell
  allocate (cell)
  cell%h = reshape ([(real(i), i = 1, 9)], [3, 3])
  call s1 (cell)
end
! { dg-final { scan-tree-dump-times "&a" 1 "original" } }
! { dg-final { scan-tree-dump-times "pack" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
