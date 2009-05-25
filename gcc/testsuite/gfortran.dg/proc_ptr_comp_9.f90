! { dg-do run }
!
! PR 40176:  Fortran 2003: Procedure pointers with array return value
!
! Original test case by Barron Bichon <barron.bichon@swri.org>
! Modified by Janus Weil <janus@gcc.gnu.org>

PROGRAM test_prog

 TYPE ProcPointerType
   PROCEDURE(triple), POINTER, NOPASS :: f
 END TYPE ProcPointerType

 TYPE (ProcPointerType) :: ppt
 PROCEDURE(triple), POINTER :: f
 REAL :: tres(2)

 ppt%f => triple
 f => ppt%f
 tres = f(2,[2.,4.])
 if (abs(tres(1)-6.)>1E-3) call abort()
 if (abs(tres(2)-12.)>1E-3) call abort()
 tres = ppt%f(2,[3.,5.])
 if (abs(tres(1)-9.)>1E-3) call abort()
 if (abs(tres(2)-15.)>1E-3) call abort()

CONTAINS

 FUNCTION triple(n,x) RESULT(tre)
   INTEGER, INTENT(in) :: n
   REAL, INTENT(in) :: x(2)
   REAL :: tre(2)
   tre = 3.*x
 END FUNCTION triple

END PROGRAM test_prog

